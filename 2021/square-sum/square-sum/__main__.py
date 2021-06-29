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


def find_end(g, num):
    for i in range(1, num+1):
        if len(g[i]) == 1:
            return i


def connect(g: dict, num):
    m = find_end(g, num)
    g[-1] = [m]
    p = find_path(g, -1, [], set(range(1, num + 1)))
    return p


def rec_add(g: dict, i: int, s: set):
    for j in g[i]:
        if j not in s:
            s.add(j)
            rec_add(g, j, s)


def find_path(g, i, l, r):
    for nxt in g[i]:
        if nxt in r:
            r.remove(nxt)
            rp = find_path(g, nxt, l + [nxt], r)
            if rp:
                return rp
            else:
                r.add(nxt)
                continue
    if len(r) != 0:
        return False
    else:
        return l


def square_sums(num):
    g = init(num)
    return connect(g, num)


if __name__ == "__main__":
    print(square_sums(37))
