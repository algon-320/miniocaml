def tarai(x, y, z):
  if x <= y:
    return y
  else:
    return tarai(tarai(x-1, y, z), tarai(y-1, z, x), tarai(z-1, x, y))

x = int(input())
y = int(input())
z = int(input())
print("{}".format(tarai(x, y, z)))
