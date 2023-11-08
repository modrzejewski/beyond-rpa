#!/usr/bin/env python3

def ProcessIntegrals(x, y):
    r = ["a", "b", "c", "d"]
    r.remove(x)
    r.remove(y)
    d = {}
    if y > x:
        d["a"] = r[0]
        d["c"] = r[1]
        d["b"] = x
        d["d"] = y
    else:
        d["a"] = r[1]
        d["c"] = r[0]
        d["b"] = y
        d["d"] = x
    for t in ["a", "b", "c", "d"]:
        X = t.upper()
        Y = d[t].upper()
        d[X] = Y
    d["X"] = x.upper()
    d["Y"] = y.upper()
    
    s = """
      pure subroutine exch_digest_Rho{X}{Y}(t, k0, k1, rho, a, na, b, nb, c, nc, d, nd, V, ShellPairLoc)
            real(F64), dimension(N{a}, N{c}), intent(out) :: T
            integer, intent(out) :: k0, k1
            real(F64), dimension(:), intent(in) :: Rho
            integer, intent(in) :: a, na
            integer, intent(in) :: b, nb
            integer, intent(in) :: c, nc
            integer, intent(in) :: d, nd
            real(F64), dimension(Nd, Nc, Nb, Na), intent(in) :: V
            integer, dimension(:, :), intent(in) :: ShellPairLoc

            integer :: r0, r1
            
            k0 = ShellPairLoc({a}, {c})
            k1 = ShellPairLoc({a}, {c}) + N{a} * N{c} - 1
            r0 = ShellPairLoc({d}, {b})
            r1 = ShellPairLoc({d}, {b}) + N{d} * N{b} - 1
            call exch_VRho{X}{Y}(T, V, Rho(r0:r1), Na, Nb, Nc, Nd)
      end subroutine exch_digest_Rho{X}{Y}


      pure subroutine exch_VRho{X}{Y}(T, V, Rho, Na, Nb, Nc, Nd)
            real(F64), dimension(N{a}, N{c}), intent(out) :: T
            real(F64), dimension(Nd, Nc, Nb, Na), intent(in) :: V
            real(F64), dimension(N{d}, N{b}), intent(in) :: Rho
            integer, intent(in) :: Na, Nb, Nc, Nd

            integer :: a, b, c, d

            T = ZERO
            do a = 1, Na
                  do b = 1, Nb
                        do c = 1, Nc
                              do d = 1, Nd
                                    T({a}, {c}) = T({a}, {c}) + Rho({d}, {b}) * V(d, c, b, a)
                              end do
                        end do
                  end do
            end do
      end subroutine exch_VRho{X}{Y}
""".format(**d)
    return s


RhoIndices = [("b", "d"), ("b", "c"), ("a", "d"), ("a", "c"), ("d", "b"), ("d", "a"), ("c", "b"), ("c", "a")]
for x, y in RhoIndices:
    print(ProcessIntegrals(x, y))
