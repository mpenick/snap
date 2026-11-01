// The collector: allocation, marking, sweeping, finalizers, pins and the
// allocation profile
#include "snap_gc.h"
#include "snap_object.h"
#include "snap_compile.h"

static const uint16_t slot_sizes[SNAP_NUM_SLOT_SIZES] = { 16, 32, 48, 64, 96, 128, 192, 256 };

/* Allocation profile */

static SnapAllocSite* alloc_site(Snap* snap, SCode* code, uint8_t type) {
  int mask, i;
  if (snap->alloc_sites_count * 4 >= snap->alloc_sites_cap * 3) {
    // Grow, and put the rows back
    SnapAllocSite* old = snap->alloc_sites;
    int old_cap = snap->alloc_sites_cap, j;
    snap->alloc_sites_cap = old_cap ? 2 * old_cap : 256;
    snap->alloc_sites = (SnapAllocSite*)calloc(snap->alloc_sites_cap, sizeof(SnapAllocSite));
    snap->alloc_sites_count = 0;
    for (j = 0; j < old_cap; ++j) {
      if (old[j].count) *alloc_site(snap, old[j].code, old[j].type) = old[j];
    }
    free(old);
  }
  mask = snap->alloc_sites_cap - 1;
  for (i = (int)((((uintptr_t)code >> 4) * 31 + type) & mask); snap->alloc_sites[i].count; i = (i + 1) & mask) {
    if (snap->alloc_sites[i].code == code && snap->alloc_sites[i].type == type) return &snap->alloc_sites[i];
  }
  snap->alloc_sites[i].code = code;
  snap->alloc_sites[i].type = type;
  snap->alloc_sites_count++;
  return &snap->alloc_sites[i];
}

void profile_alloc(Snap* snap, uint8_t type, size_t size) {
  SnapAllocSite* site = alloc_site(snap, snap->frames.vsize > 0 ? mvec_back(&snap->frames).code : NULL, type);
  site->count++;
  site->bytes += size;
}

// A function's code is being freed: its rows go to "<freed>", so new code
// at the same address doesn't take them
static void profile_code_freed(Snap* snap, SCode* code) {
  int i;
  for (i = 0; i < snap->alloc_sites_cap; ++i) {
    SnapAllocSite* site = &snap->alloc_sites[i];
    if (site->count && site->code == code) {
      SnapAllocSite moved = *site;
      int j, mask = snap->alloc_sites_cap - 1;
      // Take it out, then put back the rows after it in the same run
      site->count = 0;
      snap->alloc_sites_count--;
      for (j = (i + 1) & mask; snap->alloc_sites[j].count; j = (j + 1) & mask) {
        SnapAllocSite again = snap->alloc_sites[j];
        snap->alloc_sites[j].count = 0;
        snap->alloc_sites_count--;
        *alloc_site(snap, again.code, again.type) = again;
      }
      {
        SnapAllocSite* to = alloc_site(snap, FREED_CODE, moved.type);
        to->count += moved.count;
        to->bytes += moved.bytes;
      }
      i = -1; // Rows moved, so look again from the start
    }
  }
}

/* GC */

// Bytes owned by a code object outside of its own allocation
size_t code_extra_bytes(SCode* code) {
  return code->insts_count * sizeof(SnapInst) +
         code->num_constants * sizeof(SValue) +
         code->num_upvals * sizeof(uint32_t) +
         code->num_caches * sizeof(SFieldCache);
}

// Returns how many buffers it freed with free(), besides the object itself
int gc_free(Snap* snap, SObject* obj) {
  int freed = 0;
  switch (obj->type) {
    case STYPE_FLOAT:
      snap->num_bytes_alloced -= sizeof(SFloat);
      break;
    case STYPE_SYM:
      snap->num_bytes_alloced -= (sizeof(SSym) + ((SSym*)obj)->len + 1);
      break;
    case STYPE_STR:
      snap->num_bytes_alloced -= (sizeof(SStr) + ((SStr*)obj)->len + 1);
      break;
    case STYPE_KEY:
      snap->num_bytes_alloced -= (sizeof(SKeyword) + ((SKeyword*)obj)->len + 1);
      break;
    case STYPE_ERR:
      snap->num_bytes_alloced -= sizeof(SErr);
      break;
    case STYPE_CONS:
      snap->num_bytes_alloced -= sizeof(SCons);
      break;
    case STYPE_VEC: {
      SVec* vec = (SVec*)obj;
      int k;
      snap->num_bytes_alloced -= sizeof(SVec) + snap_vec_cap(vec) * sizeof(SValue);
      if (vec->data && vec->data != vec_inline(vec)) {
        free(vec->data);
        freed++;
      }
      if (vec->segs) {
        for (k = 0; k < vec->num_segs; ++k) free(vec->segs[k]);
        free(vec->segs);
        freed += vec->num_segs + 1;
      }
      break;
    }
    case STYPE_MAP:
      snap->num_bytes_alloced -= sizeof(SMap) + map_bytes(((SMap*)obj)->cap);
      free(((SMap*)obj)->entries);
      free(((SMap*)obj)->index);
      freed = 2;
      break;
    case STYPE_CODE:
      if (snap->alloc_sites_count > 0) profile_code_freed(snap, (SCode*)obj);
      snap->num_bytes_alloced -= sizeof(SCode) + code_extra_bytes((SCode*)obj);
      free(((SCode*)obj)->insts);
      free(((SCode*)obj)->constants);
      free(((SCode*)obj)->upvals);
      free(((SCode*)obj)->caches);
      freed = 4;
      break;
    case STYPE_CLOSURE:
      snap->num_bytes_alloced -= (sizeof(SClosure) + ((SClosure*)obj)->num_upvals * sizeof(SUpval*));
      break;
    case STYPE_UPVAL:
      snap->num_bytes_alloced -= sizeof(SUpval);
      break;
    case STYPE_TYPE:
      snap->num_bytes_alloced -= sizeof(SType) + ((SType*)obj)->num_fields * sizeof(SValue);
      break;
    case STYPE_STRUCT:
      snap->num_bytes_alloced -= sizeof(SStruct) + ((SStruct*)obj)->num_vals * sizeof(SValue);
      break;
    case STYPE_METHOD:
      snap->num_bytes_alloced -= sizeof(SMethod);
      break;
    default:
      assert(0 && "Not a GC object");
      break;
  }
  return freed;
}

void gc_add_page(Snap* snap, SnapPage* page) {
  page->next = snap->pages;
  snap->pages = page;
  // A sweep on the first page stays on it (new pages hold no garbage)
  if (snap->gc_sweep == &snap->pages) snap->gc_sweep = &page->next;
}

// Threads a new page's slots onto the free list for slot size c, first slot
// first
void gc_new_page(Snap* snap, int c) {
  uint32_t size = slot_sizes[c], i, n = (GC_PAGE_SIZE - sizeof(SnapPage)) / size;
  SnapPage* page = (SnapPage*)malloc(sizeof(SnapPage) + n * size);
  page->slot_size = size;
  page->num_slots = n;
  gc_add_page(snap, page);
  for (i = n; i-- > 0;) {
    SFreeSlot* slot = (SFreeSlot*)page_slot(page, i);
    slot->type = SLOT_FREE;
    slot->next = snap->free_slots[c];
    snap->free_slots[c] = (SObject*)slot;
  }
}

// Picks the next chunk [*start, *end) of up to GC_CHUNK of count children,
// from gray->pos, and moves gray->pos past it (or to -1 when it's the last)
static void gc_chunk(SnapGray* gray, int count, int* start, int* end) {
  *start = gray->pos < count ? gray->pos : count; // A vector can shrink
  *end = count - *start > GC_CHUNK ? *start + GC_CHUNK : count;
  gray->pos = *end < count ? *end : -1;
}

// Marks the next chunk of vals. Returns the work done.
static inline int gc_scan_vals(Snap* snap, SnapGray* gray, SValue* vals, int count) {
  int start, end;
  gc_chunk(gray, count, &start, &end);
  gc_mark_vals(snap, vals + start, end - start);
  return end - start + 1;
}

// The index after the end of the segment holding vector index i
static int vec_seg_end(SVec* vec, int i) {
  int k = 63 - __builtin_clzll((uint64_t)(i - vec->cap0) + SNAP_VEC_SEG) - SNAP_VEC_SEG_BITS;
  return vec->cap0 + (SNAP_VEC_SEG << (k + 1)) - SNAP_VEC_SEG;
}

// Marks the children of a gray object, a chunk at a time for the ones that
// can be big. Vectors and maps can change between chunks: positions are
// indexes, removed values go through the barrier, and map_rebuild() marks
// the entries it moves.
static int gc_scan(Snap* snap, SnapGray* gray) {
  SObject* obj = gray->obj;
  int i;
  switch (obj->type) {
    case STYPE_VEC: {
      SVec* vec = (SVec*)obj;
      int start, end, run;
      gc_chunk(gray, vec->len, &start, &end);
      for (i = start; i < end; i = run) { // A run of values in one array at a time
        run = i < vec->cap0 ? vec->cap0 : vec_seg_end(vec, i);
        if (run > end) run = end;
        gc_mark_vals(snap, snap_vec_at(vec, i), run - i);
      }
      return end - start + 1;
    }
    case STYPE_MAP:
      return gc_scan_vals(snap, gray, ((SMap*)obj)->entries, 2 * ((SMap*)obj)->used); // Deleted keys aren't objects
    case STYPE_CODE:
      return gc_scan_vals(snap, gray, ((SCode*)obj)->constants, ((SCode*)obj)->num_constants);
    case STYPE_ERR:
      gc_mark_val(snap, ((SErr*)obj)->err);
      gc_mark_val(snap, ((SErr*)obj)->msg);
      break;
    case STYPE_CONS:
      gc_mark_val(snap, ((SCons*)obj)->first);
      gc_mark_val(snap, ((SCons*)obj)->rest);
      break;
    case STYPE_CLOSURE:
      gc_mark(snap, (SObject*)((SClosure*)obj)->code);
      for (i = 0; i < ((SClosure*)obj)->num_upvals; ++i) {
        gc_mark(snap, (SObject*)((SClosure*)obj)->upvals[i]);
      }
      gray->pos = -1;
      return i + 1;
    case STYPE_UPVAL:
      gc_mark_val(snap, *((SUpval*)obj)->value);
      break;
    case STYPE_STRUCT: // Its type is never freed
      return gc_scan_vals(snap, gray, ((SStruct*)obj)->vals, ((SStruct*)obj)->stype->num_fields);
    case STYPE_METHOD:
      gc_mark(snap, (SObject*)((SMethod*)obj)->table);
      break;
    default:
      assert(0 && "Not a valid gray object");
      break;
  }
  gray->pos = -1;
  return 2;
}


// Starts a collection by flipping which mark value means unmarked, so that
// every object is unmarked without touching them, then marks the roots. The
// stack is marked all at once here, which is what lets stack stores skip the
// barrier. Symbols (and so globals) are marked in steps instead.
static void gc_start(Snap* snap) {
  SObject** obj;
  SnapFrame* frame;
  SUpval* upval;

  snap->gc_phase = GC_MARK;
  snap->gc_unmarked ^= 1;
  snap->gc_sym = 0;
  snap->gc_separated = false;
  snap->gc_fin_pos = snap->gc_fin_kept = 0;
  snap->gc_debt = 0;
  snap->gc_live = snap->num_bytes_alloced; // Less what the sweep frees

  mvec_foreach(&snap->anchors, obj) {
    gc_mark(snap, *obj);
  }
  gc_mark_vals(snap, snap->pins.vitems, snap->pins.vsize);
  gc_mark_vals(snap, snap->tobefnz.vitems, snap->tobefnz.vsize); // Waiting for their finalizers

  gc_mark(snap, (SObject*)snap->cause);

  // Each frame's [stack_base, stack_top) region is live (args, locals and
  // temporaries); the top frame's stack_top is synced before any allocation.
  mvec_foreach(&snap->frames, frame) {
    gc_mark(snap, (SObject*)frame->code);
    gc_mark(snap, (SObject*)frame->closure);
    gc_mark_vals(snap, frame->stack_base, frame->stack_top - frame->stack_base);
  }

  for (upval = snap->open_upvals; upval; upval = upval->next) {
    gc_mark(snap, (SObject*)upval);
  }

  gc_mark_fn_states(snap);
}

// Once marking has reached everything, moves the structs with finalizers that
// it didn't reach from finobj to tobefnz, in the order they were made, and
// marks them. They (and what they reach) live until their finalizers have run,
// which happens at the VM's next safe point, as allocation can't run snap
// code. This looks at GC_CHUNK * 4 of finobj per call, keeping the marked
// ones at its front and gathering the unmarked ones in gc_dying, until it
// reaches the end. That's safe across steps: once marking has reached
// everything, nothing can reach an unmarked struct, and structs made since are
// marked (and pushed on the end, so they're looked at too). Returns the work
// done.
static size_t gc_separate(Snap* snap) {
  int i, end = snap->gc_fin_pos + GC_CHUNK * 4, first;
  size_t done;
  if (end > snap->finobj.vsize) end = snap->finobj.vsize;
  for (i = snap->gc_fin_pos; i < end; ++i) {
    SObject* obj = snap->finobj.vitems[i];
    if (obj->mark == snap->gc_unmarked) *mvec_push(&snap->gc_dying, SObject*) = obj;
    else snap->finobj.vitems[snap->gc_fin_kept++] = obj;
  }
  done = end - snap->gc_fin_pos + 1;
  snap->gc_fin_pos = end;
  if (end < snap->finobj.vsize) return done;
  snap->finobj.vsize = snap->gc_fin_kept;
  // Marking them after moving them all finalizes a struct that's only
  // reachable from another one's too, as in Lua
  first = snap->tobefnz.vsize;
  for (i = 0; i < snap->gc_dying.vsize; ++i) {
    *mvec_push(&snap->tobefnz, SValue) = create_object(snap->gc_dying.vitems[i]);
  }
  for (i = first; i < snap->tobefnz.vsize; ++i) gc_mark_val(snap, snap->tobefnz.vitems[i]);
  done += snap->gc_dying.vsize;
  snap->gc_dying.vsize = 0;
  snap->gc_fin_pos = snap->gc_fin_kept = 0;
  if (snap->tobefnz.vsize > first) {
    snap->fin_pending = true;
    snap->reentered = true; // Gets the VM to look at fin_pending after its next C call
  }
  snap->gc_separated = true;
  return done;
}

// Does about `work` units of marking or sweeping. Returns true when it
// finishes the collection. With a start time it stops once the step has taken
// GC_STEP_BUDGET, and sets *left to the units it didn't do.
static bool gc_step(Snap* snap, size_t work, const struct timespec* t0, size_t* left) {
  size_t done = 0, check = GC_CLOCK_UNITS;
  while (done < work) {
    if (GC_STEP_BUDGET > 0 && t0 && done >= check) {
      struct timespec t;
      check = done + GC_CLOCK_UNITS;
      clock_gettime(CLOCK_MONOTONIC, &t);
      if ((t.tv_sec - t0->tv_sec) * 1000000000L + (t.tv_nsec - t0->tv_nsec) > GC_STEP_BUDGET &&
          work - done <= GC_MAX_CARRY) {
        *left = work - done;
        return false;
      }
    }
    if (snap->gc_phase == GC_MARK) {
      if (snap->gc_sym < snap->symbols.hcapacity) {
        // Symbols and keywords are never freed (GC_FIXED), but symbols hold
        // the globals. A resize of the table restarts this (see snap_sym_new()).
        SnapHashEntry* entry = &snap->symbols.hentries[snap->gc_sym++];
        if (!is_undef(entry->hkey)) gc_mark_val(snap, as_sym(entry->hkey)->value);
        done++;
      } else if (snap->gray.vsize > 0) {
        SnapGray gray = snap->gray.vitems[--snap->gray.vsize];
        done += gc_scan(snap, &gray);
        if (gray.pos >= 0) *mvec_push(&snap->gray, SnapGray) = gray;
      } else if (!snap->gc_separated) {
        done += gc_separate(snap); // Can gray more objects, so marking goes on
      } else {
        snap->gc_phase = GC_SWEEP;
        snap->gc_sweep = &snap->pages;
        snap->gc_sweep_slot = 0;
      }
    } else if (snap->gc_phase == GC_SWEEP) {
      // Objects made during the sweep are marked, wherever they go
      SnapPage* page = *snap->gc_sweep;
      SObject* obj;
      if (!page) {
        // Grow from the live estimate rather than the heap size, which
        // includes everything allocated during the collection
        snap->gc_phase = GC_IDLE;
        snap->next_gc = snap->gc_live * GC_HEAP_GROWTH;
        if (snap->next_gc < snap->num_bytes_alloced + GC_MIN_STEP) {
          snap->next_gc = snap->num_bytes_alloced + GC_MIN_STEP;
        }
        return true;
      }
      obj = page_slot(page, snap->gc_sweep_slot);
      done++;
      if (obj->type != SLOT_FREE && obj->mark == snap->gc_unmarked) {
        size_t before = snap->num_bytes_alloced;
        done += gc_free(snap, obj) * GC_FREE_COST;
        snap->gc_live -= before - snap->num_bytes_alloced;
        if (page->num_slots == 1) { // An object's own page
          done += GC_FREE_COST;
          *snap->gc_sweep = page->next;
          free(page);
          continue;
        } else {
          int c = slot_size_index[page->slot_size / 16];
          obj->type = SLOT_FREE;
          ((SFreeSlot*)obj)->next = snap->free_slots[c];
          snap->free_slots[c] = obj;
        }
      }
      if (++snap->gc_sweep_slot == page->num_slots) {
        snap->gc_sweep = &page->next;
        snap->gc_sweep_slot = 0;
      }
    } else {
      break;
    }
  }
  return false;
}

// Starts a collection and/or does a step of one, timing it for the stats.
// With left, the step keeps to GC_STEP_BUDGET (see gc_step()).
bool gc_pause(Snap* snap, bool start, size_t work, size_t* left) {
  SnapGCStats* stats = &snap->gc_stats;
  struct timespec t0, t1;
  bool done = false;
  double t;
  clock_gettime(CLOCK_MONOTONIC, &t0);
  if (start) gc_start(snap);
  if (work > 0) done = gc_step(snap, work, left ? &t0 : NULL, left);
  clock_gettime(CLOCK_MONOTONIC, &t1);
  t = (double)(t1.tv_sec - t0.tv_sec) + (double)(t1.tv_nsec - t0.tv_nsec) * 1e-9;
  stats->recent[stats->steps % SNAP_GC_RECENT] = t;
  stats->steps++;
  stats->pause_total += t;
  if (t > stats->pause_max) stats->pause_max = t;
  if (done) stats->collections++;
  return done;
}

bool snap_gc_step(Snap* snap, size_t work) {
  return gc_pause(snap, snap->gc_phase == GC_IDLE, work, NULL);
}

SObject* snap_anchor(Snap* snap, SObject* obj) {
  return anchor(snap, obj);
}

void snap_release(Snap* snap) {
  release(snap);
}

// Pins are roots, marked when a collection starts. A value pinned during the
// mark needs no barrier: it was either reachable then, so it gets marked, or
// it's new, and new objects are already marked.
SValue snap_pin(Snap* snap, SValue val) {
  if (is_obj(val)) *mvec_push(&snap->pins, SValue) = val;
  return val;
}

// Removes the latest pin of val, by identity, moving the last pin into its place
void snap_unpin(Snap* snap, SValue val) {
  int i;
  for (i = snap->pins.vsize - 1; i >= 0; --i) {
    if (snap->pins.vitems[i].bits == val.bits) {
      snap->pins.vitems[i] = snap->pins.vitems[--snap->pins.vsize];
      return;
    }
  }
}

void snap_set_gc(Snap* snap, SType* type, SValue fn) {
  assert(type->is_struct);
  snap_unpin(snap, type->gc);
  type->gc = snap_pin(snap, fn);
}

void snap_run_finalizers(Snap* snap) {
  if (snap->fin_running || snap->c_depth >= SNAP_MAX_C_DEPTH) return;
  snap->fin_running = true;
  while (snap->tobefnz.vsize > 0) {
    // Popped and pinned, so it's kept for the call, then left for the
    // collector unless the finalizer kept it. It's never finalized again.
    SValue obj = snap->tobefnz.vitems[--snap->tobefnz.vsize], r;
    SType* type = as_struct(obj)->stype;
    if (is_nil(type->gc)) continue; // Taken away since
    snap_pin(snap, obj);
    r = snap_call(snap, type->gc, &obj, 1);
    if (is_err(r)) {
      // There's no one to return the error to, so it's a warning, as in Lua
      fflush(stdout); // After what the program printed before it
      fprintf(stderr, "snap: finalizer for %s failed: ", type->name->data);
      print_val(stderr, r);
      fputc('\n', stderr);
    }
    snap_unpin(snap, obj);
  }
  snap->fin_pending = false;
  snap->fin_running = false;
}
