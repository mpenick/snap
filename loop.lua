

-- Same as loop.snap: both loops go from 0 while less than 10000
local s = 0
for i = 0, 10000 - 1 do
  for j = 0, 10000 - 1 do
    s = s + i * j
  end
end

print(s)

