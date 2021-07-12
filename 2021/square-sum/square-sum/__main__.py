from math import sqrt


def init(num):
    g = dict()
    for i in range(1, num + 1):
        g[i] = list()
        for s in range(2, int(sqrt(2 * num) + 1)):
            compl = s ** 2 - i
            if compl > 0 and compl <= num and compl != i:
                g[i].append(compl)
    return g

def square_sums(n):
    g = init(n)
    def dfs():
        if not inp: yield res
        for v in tuple(inp):
            if not res or (res[-1] in g[v]):
                res.append(v)
                inp.discard(v)
                yield from dfs()
                inp.add(res.pop())

    inp, res = set(range(1,n+1)), []
    return next(dfs(), False)

if __name__ == "__main__":
    print(square_sums(15))
