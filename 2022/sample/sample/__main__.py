from math import sqrt
from matplotlib import pyplot as plt
from random import uniform, randrange
import cmath
from math import sqrt


def generate_dot_in_square():
    return (uniform(0, 1), uniform(0, 1))


def generate_dot():
    a, b = generate_dot_in_square()
    c = complex((a + b / 2) / sqrt(3.0), b / 2.0)
    c *= cmath.rect(1.0, cmath.pi / 3.0 * randrange(0, 6))
    return c.real, c.imag


plt.xlim([-1, 1])
plt.ylim([-1, 1])
plt.axis('equal')
for _ in range(1000):
    x, y = generate_dot()
    plt.plot(x, y, 'go')

plt.show()
