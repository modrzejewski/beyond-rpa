module cisd22

  use ccsd_transformed_integrals
  use basis

  implicit none

contains

  function cisd22_aibjakbl(i, j, k, l) 
    double precision :: cisd22_aibjakbl   
    integer, intent(in) :: i, j, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + oooo(j, l, i, k)



    cisd22_aibjakbl = 0.d+0
    do s = 0, 0
       cisd22_aibjakbl = cisd22_aibjakbl + term(s)
    end do

  end function cisd22_aibjakbl
  function cisd22_aibjakdi(b, j, k, d) 
    double precision :: cisd22_aibjakdi   
    integer, intent(in) :: b, j, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(d, k, b, j)

    term(0) = -term(0) 


    cisd22_aibjakdi = 0.d+0
    do s = 0, 0
       cisd22_aibjakdi = cisd22_aibjakdi + term(s)
    end do

  end function cisd22_aibjakdi
  function cisd22_aibjakdj(i, b, k, d) 
    double precision :: cisd22_aibjakdj   
    integer, intent(in) :: i, b, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(b, d, i, k)

    term(0) = -term(0) 


    cisd22_aibjakdj = 0.d+0
    do s = 0, 0
       cisd22_aibjakdj = cisd22_aibjakdj + term(s)
    end do

  end function cisd22_aibjakdj
  function cisd22_aibjaidl(b, j, d, l) 
    double precision :: cisd22_aibjaidl   
    integer, intent(in) :: b, j, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(b, d, j, l)
    term(1) = term(1) + vovo(d, l, b, j)

    term(0) = -term(0) 
    term(1) = term(1) * 1.9999999999999998d+0 


    cisd22_aibjaidl = 0.d+0
    do s = 0, 1
       cisd22_aibjaidl = cisd22_aibjaidl + term(s)
    end do

  end function cisd22_aibjaidl
  function cisd22_aibjbkdi(a, j, k, d) 
    double precision :: cisd22_aibjbkdi   
    integer, intent(in) :: a, j, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, d, j, k)

    term(0) = -term(0) 


    cisd22_aibjbkdi = 0.d+0
    do s = 0, 0
       cisd22_aibjbkdi = cisd22_aibjbkdi + term(s)
    end do

  end function cisd22_aibjbkdi
  function cisd22_aibjbkdj(a, i, k, d) 
    double precision :: cisd22_aibjbkdj   
    integer, intent(in) :: a, i, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(d, k, a, i)

    term(0) = -term(0) 


    cisd22_aibjbkdj = 0.d+0
    do s = 0, 0
       cisd22_aibjbkdj = cisd22_aibjbkdj + term(s)
    end do

  end function cisd22_aibjbkdj
  function cisd22_aibjbjdl(a, i, d, l) 
    double precision :: cisd22_aibjbjdl   
    integer, intent(in) :: a, i, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, d, i, l)
    term(1) = term(1) + vovo(d, l, a, i)

    term(0) = -term(0) 
    term(1) = term(1) * 1.9999999999999998d+0 


    cisd22_aibjbjdl = 0.d+0
    do s = 0, 1
       cisd22_aibjbjdl = cisd22_aibjbjdl + term(s)
    end do

  end function cisd22_aibjbjdl
  function cisd22_aibjckai(b, j, c, k) 
    double precision :: cisd22_aibjckai   
    integer, intent(in) :: b, j, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(b, c, j, k)
    term(1) = term(1) + vovo(c, k, b, j)

    term(0) = -term(0) 
    term(1) = term(1) * 1.9999999999999998d+0 


    cisd22_aibjckai = 0.d+0
    do s = 0, 1
       cisd22_aibjckai = cisd22_aibjckai + term(s)
    end do

  end function cisd22_aibjckai
  function cisd22_aibjcial(b, j, c, l) 
    double precision :: cisd22_aibjcial   
    integer, intent(in) :: b, j, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(c, l, b, j)

    term(0) = -term(0) 


    cisd22_aibjcial = 0.d+0
    do s = 0, 0
       cisd22_aibjcial = cisd22_aibjcial + term(s)
    end do

  end function cisd22_aibjcial
  function cisd22_aibjcjal(i, b, c, l) 
    double precision :: cisd22_aibjcjal   
    integer, intent(in) :: i, b, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(b, c, i, l)

    term(0) = -term(0) 


    cisd22_aibjcjal = 0.d+0
    do s = 0, 0
       cisd22_aibjcjal = cisd22_aibjcjal + term(s)
    end do

  end function cisd22_aibjcjal
  function cisd22_aibjckbj(a, i, c, k) 
    double precision :: cisd22_aibjckbj   
    integer, intent(in) :: a, i, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, c, i, k)
    term(1) = term(1) + vovo(c, k, a, i)

    term(0) = -term(0) 
    term(1) = term(1) * 1.9999999999999998d+0 


    cisd22_aibjckbj = 0.d+0
    do s = 0, 1
       cisd22_aibjckbj = cisd22_aibjckbj + term(s)
    end do

  end function cisd22_aibjckbj
  function cisd22_aibjcibl(a, j, c, l) 
    double precision :: cisd22_aibjcibl   
    integer, intent(in) :: a, j, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, c, j, l)

    term(0) = -term(0) 


    cisd22_aibjcibl = 0.d+0
    do s = 0, 0
       cisd22_aibjcibl = cisd22_aibjcibl + term(s)
    end do

  end function cisd22_aibjcibl
  function cisd22_aibjcjbl(a, i, c, l) 
    double precision :: cisd22_aibjcjbl   
    integer, intent(in) :: a, i, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(c, l, a, i)

    term(0) = -term(0) 


    cisd22_aibjcjbl = 0.d+0
    do s = 0, 0
       cisd22_aibjcjbl = cisd22_aibjcjbl + term(s)
    end do

  end function cisd22_aibjcjbl
  function cisd22_aibjcjdi(a, b, c, d) 
    double precision :: cisd22_aibjcjdi   
    integer, intent(in) :: a, b, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) ! + vvvv(b, c, a, d)



    cisd22_aibjcjdi = 0.d+0
    do s = 0, 0
       cisd22_aibjcjdi = cisd22_aibjcjdi + term(s)
    end do

  end function cisd22_aibjcjdi
  function cisd22_aibjcidj(a, b, c, d) 
    double precision :: cisd22_aibjcidj   
    integer, intent(in) :: a, b, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) ! + vvvv(b, d, a, c)



    cisd22_aibjcidj = 0.d+0
    do s = 0, 0
       cisd22_aibjcidj = cisd22_aibjcidj + term(s)
    end do

  end function cisd22_aibjcidj
  function cisd22_aiajakal(i, j, k, l) 
    double precision :: cisd22_aiajakal   
    integer, intent(in) :: i, j, k, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + oooo(j, l, i, k)
    term(1) = term(1) + oooo(j, k, i, l)



    cisd22_aiajakal = 0.d+0
    do s = 0, 1
       cisd22_aiajakal = cisd22_aiajakal + term(s)
    end do

  end function cisd22_aiajakal
  function cisd22_aibjakai(a, b, j, k) 
    double precision :: cisd22_aibjakai   
    integer, intent(in) :: a, b, j, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, b, j, k)
    term(1) = term(1) + vovo(b, j, a, k)

    term(0) = -term(0) 


    cisd22_aibjakai = 0.d+0
    do s = 0, 1
       cisd22_aibjakai = cisd22_aibjakai + term(s)
    end do

  end function cisd22_aibjakai
  function cisd22_aibjakaj(a, i, b, k) 
    double precision :: cisd22_aibjakaj   
    integer, intent(in) :: a, i, b, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, b, i, k)

    term(0) = -term(0) 


    cisd22_aibjakaj = 0.d+0
    do s = 0, 0
       cisd22_aibjakaj = cisd22_aibjakaj + term(s)
    end do

  end function cisd22_aibjakaj
  function cisd22_aibjaial(a, b, j, l) 
    double precision :: cisd22_aibjaial   
    integer, intent(in) :: a, b, j, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, b, j, l)
    term(1) = term(1) + vovo(b, j, a, l)

    term(0) = -term(0) 


    cisd22_aibjaial = 0.d+0
    do s = 0, 1
       cisd22_aibjaial = cisd22_aibjaial + term(s)
    end do

  end function cisd22_aibjaial
  function cisd22_aibjajal(a, i, b, l) 
    double precision :: cisd22_aibjajal   
    integer, intent(in) :: a, i, b, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, b, i, l)

    term(0) = -term(0) 


    cisd22_aibjajal = 0.d+0
    do s = 0, 0
       cisd22_aibjajal = cisd22_aibjajal + term(s)
    end do

  end function cisd22_aibjajal
  function cisd22_aiajakdi(a, j, k, d) 
    double precision :: cisd22_aiajakdi   
    integer, intent(in) :: a, j, k, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, d, j, k)
    term(1) = term(1) + vovo(d, k, a, j)

    term(0) = -term(0) 
    term(1) = -term(1) 


    cisd22_aiajakdi = 0.d+0
    do s = 0, 1
       cisd22_aiajakdi = cisd22_aiajakdi + term(s)
    end do

  end function cisd22_aiajakdi
  function cisd22_aiajakdj(a, i, k, d) 
    double precision :: cisd22_aiajakdj   
    integer, intent(in) :: a, i, k, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, d, i, k)
    term(1) = term(1) + vovo(d, k, a, i)

    term(0) = -term(0) 
    term(1) = -term(1) 


    cisd22_aiajakdj = 0.d+0
    do s = 0, 1
       cisd22_aiajakdj = cisd22_aiajakdj + term(s)
    end do

  end function cisd22_aiajakdj
  function cisd22_aiajaidl(a, j, d, l) 
    double precision :: cisd22_aiajaidl   
    integer, intent(in) :: a, j, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, d, j, l)
    term(1) = term(1) + vovo(d, l, a, j)

    term(0) = -term(0) 
    term(1) = term(1) * 1.9999999999999998d+0 


    cisd22_aiajaidl = 0.d+0
    do s = 0, 1
       cisd22_aiajaidl = cisd22_aiajaidl + term(s)
    end do

  end function cisd22_aiajaidl
  function cisd22_aiajajdl(a, i, d, l) 
    double precision :: cisd22_aiajajdl   
    integer, intent(in) :: a, i, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, d, i, l)
    term(1) = term(1) + vovo(d, l, a, i)

    term(0) = -term(0) 
    term(1) = term(1) * 1.9999999999999998d+0 


    cisd22_aiajajdl = 0.d+0
    do s = 0, 1
       cisd22_aiajajdl = cisd22_aiajajdl + term(s)
    end do

  end function cisd22_aiajajdl
  function cisd22_aibjakbi(i, b, j, k) 
    double precision :: cisd22_aibjakbi   
    integer, intent(in) :: i, b, j, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(b, k, b, j)
    term(1) = term(1) + oooo(i, k, i, j)

    term(0) = -term(0) 


    cisd22_aibjakbi = 0.d+0
    do s = 0, 1
       cisd22_aibjakbi = cisd22_aibjakbi + term(s)
    end do

  end function cisd22_aibjakbi
  function cisd22_aibjakbj(a, i, b, j, k) 
    double precision :: cisd22_aibjakbj   
    integer, intent(in) :: a, i, b, j, k 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, a, i, k)
    term(1) = term(1) + vvoo(b, b, i, k)
    term(2) = term(2) + vovo(a, k, a, i)
    term(3) = term(3) + oooo(j, j, i, k)

    term(0) = -term(0) 
    term(1) = -term(1) 
    term(2) = term(2) * 1.9999999999999998d+0 


    cisd22_aibjakbj = 0.d+0
    do s = 0, 3
       cisd22_aibjakbj = cisd22_aibjakbj + term(s)
    end do

  end function cisd22_aibjakbj
  function cisd22_aibjakbk(i, j, k) 
    double precision :: cisd22_aibjakbk   
    integer, intent(in) :: i, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + oooo(j, k, i, k)



    cisd22_aibjakbk = 0.d+0
    do s = 0, 0
       cisd22_aibjakbk = cisd22_aibjakbk + term(s)
    end do

  end function cisd22_aibjakbk
  function cisd22_aibiakbl(i, k, l) 
    double precision :: cisd22_aibiakbl   
    integer, intent(in) :: i, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + oooo(i, l, i, k)



    cisd22_aibiakbl = 0.d+0
    do s = 0, 0
       cisd22_aibiakbl = cisd22_aibiakbl + term(s)
    end do

  end function cisd22_aibiakbl
  function cisd22_aibjaibl(a, i, b, j, l) 
    double precision :: cisd22_aibjaibl   
    integer, intent(in) :: a, i, b, j, l 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, a, j, l)
    term(1) = term(1) + vvoo(b, b, j, l)
    term(2) = term(2) + vovo(b, l, b, j)
    term(3) = term(3) + oooo(j, l, i, i)

    term(0) = -term(0) 
    term(1) = -term(1) 
    term(2) = term(2) * 1.9999999999999998d+0 


    cisd22_aibjaibl = 0.d+0
    do s = 0, 3
       cisd22_aibjaibl = cisd22_aibjaibl + term(s)
    end do

  end function cisd22_aibjaibl
  function cisd22_aibjajbl(a, i, j, l) 
    double precision :: cisd22_aibjajbl   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(a, l, a, i)
    term(1) = term(1) + oooo(j, l, i, j)

    term(0) = -term(0) 


    cisd22_aibjajbl = 0.d+0
    do s = 0, 1
       cisd22_aibjajbl = cisd22_aibjajbl + term(s)
    end do

  end function cisd22_aibjajbl
  function cisd22_aibiakdi(i, b, k, d) 
    double precision :: cisd22_aibiakdi   
    integer, intent(in) :: i, b, k, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(b, d, i, k)
    term(1) = term(1) + vovo(d, k, b, i)

    term(0) = -term(0) 
    term(1) = -term(1) 


    cisd22_aibiakdi = 0.d+0
    do s = 0, 1
       cisd22_aibiakdi = cisd22_aibiakdi + term(s)
    end do

  end function cisd22_aibiakdi
  function cisd22_aibjaidi(i, b, j, d) 
    double precision :: cisd22_aibjaidi   
    integer, intent(in) :: i, b, j, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(b, d, i, j)
    term(1) = term(1) + vovo(d, i, b, j)

    term(0) = -term(0) 


    cisd22_aibjaidi = 0.d+0
    do s = 0, 1
       cisd22_aibjaidi = cisd22_aibjaidi + term(s)
    end do

  end function cisd22_aibjaidi
  function cisd22_aibjajdi(a, b, j, d) 
    double precision :: cisd22_aibjajdi   
    integer, intent(in) :: a, b, j, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) ! + vvvv(a, d, a, b)
    term(1) = term(1) + vovo(d, j, b, j)

    term(1) = -term(1) 


    cisd22_aibjajdi = 0.d+0
    do s = 0, 1
       cisd22_aibjajdi = cisd22_aibjajdi + term(s)
    end do

  end function cisd22_aibjajdi
  function cisd22_aibjajdj(i, b, j, d) 
    double precision :: cisd22_aibjajdj   
    integer, intent(in) :: i, b, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(b, d, i, j)

    term(0) = -term(0) 


    cisd22_aibjajdj = 0.d+0
    do s = 0, 0
       cisd22_aibjajdj = cisd22_aibjajdj + term(s)
    end do

  end function cisd22_aibjajdj
  function cisd22_aibjaidj(a, i, b, j, d) 
    double precision :: cisd22_aibjaidj   
    integer, intent(in) :: a, i, b, j, d 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) ! + vvvv(b, d, a, a)
    term(1) = term(1) + vvoo(b, d, i, i)
    term(2) = term(2) + vvoo(b, d, j, j)
    term(3) = term(3) + vovo(d, j, b, j)

    term(1) = -term(1) 
    term(2) = -term(2) 
    term(3) = term(3) * 1.9999999999999998d+0 


    cisd22_aibjaidj = 0.d+0
    do s = 0, 3
       cisd22_aibjaidj = cisd22_aibjaidj + term(s)
    end do

  end function cisd22_aibjaidj
  function cisd22_aibiaidl(i, b, d, l) 
    double precision :: cisd22_aibiaidl   
    integer, intent(in) :: i, b, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(b, d, i, l)
    term(1) = term(1) + vovo(d, l, b, i)

    term(0) = -term(0) 
    term(1) = term(1) * 1.9999999999999998d+0 


    cisd22_aibiaidl = 0.d+0
    do s = 0, 1
       cisd22_aibiaidl = cisd22_aibiaidl + term(s)
    end do

  end function cisd22_aibiaidl
  function cisd22_aibjbkbi(a, b, j, k) 
    double precision :: cisd22_aibjbkbi   
    integer, intent(in) :: a, b, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, b, j, k)

    term(0) = -term(0) 


    cisd22_aibjbkbi = 0.d+0
    do s = 0, 0
       cisd22_aibjbkbi = cisd22_aibjbkbi + term(s)
    end do

  end function cisd22_aibjbkbi
  function cisd22_aibjbkbj(a, i, b, k) 
    double precision :: cisd22_aibjbkbj   
    integer, intent(in) :: a, i, b, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, b, i, k)
    term(1) = term(1) + vovo(b, k, a, i)

    term(0) = -term(0) 


    cisd22_aibjbkbj = 0.d+0
    do s = 0, 1
       cisd22_aibjbkbj = cisd22_aibjbkbj + term(s)
    end do

  end function cisd22_aibjbkbj
  function cisd22_aibjbibl(a, b, j, l) 
    double precision :: cisd22_aibjbibl   
    integer, intent(in) :: a, b, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, b, j, l)

    term(0) = -term(0) 


    cisd22_aibjbibl = 0.d+0
    do s = 0, 0
       cisd22_aibjbibl = cisd22_aibjbibl + term(s)
    end do

  end function cisd22_aibjbibl
  function cisd22_aibjbjbl(a, i, b, l) 
    double precision :: cisd22_aibjbjbl   
    integer, intent(in) :: a, i, b, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, b, i, l)
    term(1) = term(1) + vovo(b, l, a, i)

    term(0) = -term(0) 


    cisd22_aibjbjbl = 0.d+0
    do s = 0, 1
       cisd22_aibjbjbl = cisd22_aibjbjbl + term(s)
    end do

  end function cisd22_aibjbjbl
  function cisd22_aibjcjci(a, b, c) 
    double precision :: cisd22_aibjcjci   
    integer, intent(in) :: a, b, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) ! + vvvv(b, c, a, c)



    cisd22_aibjcjci = 0.d+0
    do s = 0, 0
       cisd22_aibjcjci = cisd22_aibjcjci + term(s)
    end do

  end function cisd22_aibjcjci
  function cisd22_aibjcicj(a, b, c) 
    double precision :: cisd22_aibjcicj   
    integer, intent(in) :: a, b, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) ! + vvvv(b, c, a, c)



    cisd22_aibjcicj = 0.d+0
    do s = 0, 0
       cisd22_aibjcicj = cisd22_aibjcicj + term(s)
    end do

  end function cisd22_aibjcicj
  function cisd22_aibibkdi(a, i, k, d) 
    double precision :: cisd22_aibibkdi   
    integer, intent(in) :: a, i, k, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, d, i, k)
    term(1) = term(1) + vovo(d, k, a, i)

    term(0) = -term(0) 
    term(1) = -term(1) 


    cisd22_aibibkdi = 0.d+0
    do s = 0, 1
       cisd22_aibibkdi = cisd22_aibibkdi + term(s)
    end do

  end function cisd22_aibibkdi
  function cisd22_aibjbidi(a, i, j, d) 
    double precision :: cisd22_aibjbidi   
    integer, intent(in) :: a, i, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, d, i, j)

    term(0) = -term(0) 


    cisd22_aibjbidi = 0.d+0
    do s = 0, 0
       cisd22_aibjbidi = cisd22_aibjbidi + term(s)
    end do

  end function cisd22_aibjbidi
  function cisd22_aibjbjdi(a, i, b, j, d) 
    double precision :: cisd22_aibjbjdi   
    integer, intent(in) :: a, i, b, j, d 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) ! + vvvv(b, b, a, d)
    term(1) = term(1) + vvoo(a, d, i, i)
    term(2) = term(2) + vvoo(a, d, j, j)
    term(3) = term(3) + vovo(d, i, a, i)

    term(1) = -term(1) 
    term(2) = -term(2) 
    term(3) = term(3) * 1.9999999999999998d+0 


    cisd22_aibjbjdi = 0.d+0
    do s = 0, 3
       cisd22_aibjbjdi = cisd22_aibjbjdi + term(s)
    end do

  end function cisd22_aibjbjdi
  function cisd22_aibjbjdj(a, i, j, d) 
    double precision :: cisd22_aibjbjdj   
    integer, intent(in) :: a, i, j, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, d, i, j)
    term(1) = term(1) + vovo(d, j, a, i)

    term(0) = -term(0) 


    cisd22_aibjbjdj = 0.d+0
    do s = 0, 1
       cisd22_aibjbjdj = cisd22_aibjbjdj + term(s)
    end do

  end function cisd22_aibjbjdj
  function cisd22_aibjbidj(a, i, b, d) 
    double precision :: cisd22_aibjbidj   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) ! + vvvv(b, d, a, b)
    term(1) = term(1) + vovo(d, i, a, i)

    term(1) = -term(1) 


    cisd22_aibjbidj = 0.d+0
    do s = 0, 1
       cisd22_aibjbidj = cisd22_aibjbidj + term(s)
    end do

  end function cisd22_aibjbidj
  function cisd22_aibibidl(a, i, d, l) 
    double precision :: cisd22_aibibidl   
    integer, intent(in) :: a, i, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, d, i, l)
    term(1) = term(1) + vovo(d, l, a, i)

    term(0) = -term(0) 
    term(1) = term(1) * 1.9999999999999998d+0 


    cisd22_aibibidl = 0.d+0
    do s = 0, 1
       cisd22_aibibidl = cisd22_aibibidl + term(s)
    end do

  end function cisd22_aibibidl
  function cisd22_aiajckai(a, j, c, k) 
    double precision :: cisd22_aiajckai   
    integer, intent(in) :: a, j, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, c, j, k)
    term(1) = term(1) + vovo(c, k, a, j)

    term(0) = -term(0) 
    term(1) = term(1) * 1.9999999999999998d+0 


    cisd22_aiajckai = 0.d+0
    do s = 0, 1
       cisd22_aiajckai = cisd22_aiajckai + term(s)
    end do

  end function cisd22_aiajckai
  function cisd22_aiajckaj(a, i, c, k) 
    double precision :: cisd22_aiajckaj   
    integer, intent(in) :: a, i, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, c, i, k)
    term(1) = term(1) + vovo(c, k, a, i)

    term(0) = -term(0) 
    term(1) = term(1) * 1.9999999999999998d+0 


    cisd22_aiajckaj = 0.d+0
    do s = 0, 1
       cisd22_aiajckaj = cisd22_aiajckaj + term(s)
    end do

  end function cisd22_aiajckaj
  function cisd22_aiajcial(a, j, c, l) 
    double precision :: cisd22_aiajcial   
    integer, intent(in) :: a, j, c, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, c, j, l)
    term(1) = term(1) + vovo(c, l, a, j)

    term(0) = -term(0) 
    term(1) = -term(1) 


    cisd22_aiajcial = 0.d+0
    do s = 0, 1
       cisd22_aiajcial = cisd22_aiajcial + term(s)
    end do

  end function cisd22_aiajcial
  function cisd22_aiajcjal(a, i, c, l) 
    double precision :: cisd22_aiajcjal   
    integer, intent(in) :: a, i, c, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, c, i, l)
    term(1) = term(1) + vovo(c, l, a, i)

    term(0) = -term(0) 
    term(1) = -term(1) 


    cisd22_aiajcjal = 0.d+0
    do s = 0, 1
       cisd22_aiajcjal = cisd22_aiajcjal + term(s)
    end do

  end function cisd22_aiajcjal
  function cisd22_aibickai(i, b, c, k) 
    double precision :: cisd22_aibickai   
    integer, intent(in) :: i, b, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(b, c, i, k)
    term(1) = term(1) + vovo(c, k, b, i)

    term(0) = -term(0) 
    term(1) = term(1) * 1.9999999999999998d+0 


    cisd22_aibickai = 0.d+0
    do s = 0, 1
       cisd22_aibickai = cisd22_aibickai + term(s)
    end do

  end function cisd22_aibickai
  function cisd22_aibjciai(i, b, j, c) 
    double precision :: cisd22_aibjciai   
    integer, intent(in) :: i, b, j, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(b, c, i, j)
    term(1) = term(1) + vovo(c, i, b, j)

    term(0) = -term(0) 


    cisd22_aibjciai = 0.d+0
    do s = 0, 1
       cisd22_aibjciai = cisd22_aibjciai + term(s)
    end do

  end function cisd22_aibjciai
  function cisd22_aibjcjai(a, i, b, j, c) 
    double precision :: cisd22_aibjcjai   
    integer, intent(in) :: a, i, b, j, c 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) ! + vvvv(b, c, a, a)
    term(1) = term(1) + vvoo(b, c, i, i)
    term(2) = term(2) + vvoo(b, c, j, j)
    term(3) = term(3) + vovo(c, j, b, j)

    term(1) = -term(1) 
    term(2) = -term(2) 
    term(3) = term(3) * 1.9999999999999998d+0 


    cisd22_aibjcjai = 0.d+0
    do s = 0, 3
       cisd22_aibjcjai = cisd22_aibjcjai + term(s)
    end do

  end function cisd22_aibjcjai
  function cisd22_aibjcjaj(i, b, j, c) 
    double precision :: cisd22_aibjcjaj   
    integer, intent(in) :: i, b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(b, c, i, j)

    term(0) = -term(0) 


    cisd22_aibjcjaj = 0.d+0
    do s = 0, 0
       cisd22_aibjcjaj = cisd22_aibjcjaj + term(s)
    end do

  end function cisd22_aibjcjaj
  function cisd22_aibjciaj(a, b, j, c) 
    double precision :: cisd22_aibjciaj   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) ! + vvvv(a, c, a, b)
    term(1) = term(1) + vovo(c, j, b, j)

    term(1) = -term(1) 


    cisd22_aibjciaj = 0.d+0
    do s = 0, 1
       cisd22_aibjciaj = cisd22_aibjciaj + term(s)
    end do

  end function cisd22_aibjciaj
  function cisd22_aibicial(i, b, c, l) 
    double precision :: cisd22_aibicial   
    integer, intent(in) :: i, b, c, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(b, c, i, l)
    term(1) = term(1) + vovo(c, l, b, i)

    term(0) = -term(0) 
    term(1) = -term(1) 


    cisd22_aibicial = 0.d+0
    do s = 0, 1
       cisd22_aibicial = cisd22_aibicial + term(s)
    end do

  end function cisd22_aibicial
  function cisd22_aiajcjdi(a, c, d) 
    double precision :: cisd22_aiajcjdi   
    integer, intent(in) :: a, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) ! + vvvv(a, d, a, c)



    cisd22_aiajcjdi = 0.d+0
    do s = 0, 0
       cisd22_aiajcjdi = cisd22_aiajcjdi + term(s)
    end do

  end function cisd22_aiajcjdi
  function cisd22_aiajcidj(a, c, d) 
    double precision :: cisd22_aiajcidj   
    integer, intent(in) :: a, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) ! + vvvv(a, d, a, c)



    cisd22_aiajcidj = 0.d+0
    do s = 0, 0
       cisd22_aiajcidj = cisd22_aiajcidj + term(s)
    end do

  end function cisd22_aiajcidj
  function cisd22_aibickbi(a, i, c, k) 
    double precision :: cisd22_aibickbi   
    integer, intent(in) :: a, i, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, c, i, k)
    term(1) = term(1) + vovo(c, k, a, i)

    term(0) = -term(0) 
    term(1) = term(1) * 1.9999999999999998d+0 


    cisd22_aibickbi = 0.d+0
    do s = 0, 1
       cisd22_aibickbi = cisd22_aibickbi + term(s)
    end do

  end function cisd22_aibickbi
  function cisd22_aibjcibi(a, i, j, c) 
    double precision :: cisd22_aibjcibi   
    integer, intent(in) :: a, i, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, c, i, j)

    term(0) = -term(0) 


    cisd22_aibjcibi = 0.d+0
    do s = 0, 0
       cisd22_aibjcibi = cisd22_aibjcibi + term(s)
    end do

  end function cisd22_aibjcibi
  function cisd22_aibjcjbi(a, i, b, c) 
    double precision :: cisd22_aibjcjbi   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) ! + vvvv(b, c, a, b)
    term(1) = term(1) + vovo(c, i, a, i)

    term(1) = -term(1) 


    cisd22_aibjcjbi = 0.d+0
    do s = 0, 1
       cisd22_aibjcjbi = cisd22_aibjcjbi + term(s)
    end do

  end function cisd22_aibjcjbi
  function cisd22_aibjcjbj(a, i, j, c) 
    double precision :: cisd22_aibjcjbj   
    integer, intent(in) :: a, i, j, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, c, i, j)
    term(1) = term(1) + vovo(c, j, a, i)

    term(0) = -term(0) 


    cisd22_aibjcjbj = 0.d+0
    do s = 0, 1
       cisd22_aibjcjbj = cisd22_aibjcjbj + term(s)
    end do

  end function cisd22_aibjcjbj
  function cisd22_aibjcibj(a, i, b, j, c) 
    double precision :: cisd22_aibjcibj   
    integer, intent(in) :: a, i, b, j, c 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) ! + vvvv(b, b, a, c)
    term(1) = term(1) + vvoo(a, c, i, i)
    term(2) = term(2) + vvoo(a, c, j, j)
    term(3) = term(3) + vovo(c, i, a, i)

    term(1) = -term(1) 
    term(2) = -term(2) 
    term(3) = term(3) * 1.9999999999999998d+0 


    cisd22_aibjcibj = 0.d+0
    do s = 0, 3
       cisd22_aibjcibj = cisd22_aibjcibj + term(s)
    end do

  end function cisd22_aibjcibj
  function cisd22_aibicibl(a, i, c, l) 
    double precision :: cisd22_aibicibl   
    integer, intent(in) :: a, i, c, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, c, i, l)
    term(1) = term(1) + vovo(c, l, a, i)

    term(0) = -term(0) 
    term(1) = -term(1) 


    cisd22_aibicibl = 0.d+0
    do s = 0, 1
       cisd22_aibicibl = cisd22_aibicibl + term(s)
    end do

  end function cisd22_aibicibl
  function cisd22_aibicidi(a, b, c, d) 
    double precision :: cisd22_aibicidi   
    integer, intent(in) :: a, b, c, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) ! + vvvv(b, d, a, c)
    term(1) = term(1) ! + vvvv(b, c, a, d)



    cisd22_aibicidi = 0.d+0
    do s = 0, 1
       cisd22_aibicidi = cisd22_aibicidi + term(s)
    end do

  end function cisd22_aibicidi
  function cisd22_aiajakai(a, i, j, k) 
    double precision :: cisd22_aiajakai   
    integer, intent(in) :: a, i, j, k 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, a, j, k)
    term(1) = term(1) + vovo(a, k, a, j)
    term(2) = term(2) + oooo(i, k, i, j)
    term(3) = term(3) + oooo(j, k, i, i)

    term(0) = term(0) * (-1.9999999999999998d+0) 


    cisd22_aiajakai = 0.d+0
    do s = 0, 3
       cisd22_aiajakai = cisd22_aiajakai + term(s)
    end do

  end function cisd22_aiajakai
  function cisd22_aiajakaj(a, i, j, k) 
    double precision :: cisd22_aiajakaj   
    integer, intent(in) :: a, i, j, k 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, a, i, k)
    term(1) = term(1) + vovo(a, k, a, i)
    term(2) = term(2) + oooo(j, j, i, k)
    term(3) = term(3) + oooo(j, k, i, j)

    term(0) = term(0) * (-1.9999999999999998d+0) 


    cisd22_aiajakaj = 0.d+0
    do s = 0, 3
       cisd22_aiajakaj = cisd22_aiajakaj + term(s)
    end do

  end function cisd22_aiajakaj
  function cisd22_aiajakak(i, j, k) 
    double precision :: cisd22_aiajakak   
    integer, intent(in) :: i, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + oooo(j, k, i, k)

    term(0) = term(0) * 1.9999999999999998d+0 


    cisd22_aiajakak = 0.d+0
    do s = 0, 0
       cisd22_aiajakak = cisd22_aiajakak + term(s)
    end do

  end function cisd22_aiajakak
  function cisd22_aiaiakal(i, k, l) 
    double precision :: cisd22_aiaiakal   
    integer, intent(in) :: i, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + oooo(i, l, i, k)



    cisd22_aiaiakal = 0.d+0
    do s = 0, 0
       cisd22_aiaiakal = cisd22_aiaiakal + term(s)
    end do

  end function cisd22_aiaiakal
  function cisd22_aiajaial(a, i, j, l) 
    double precision :: cisd22_aiajaial   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, a, j, l)
    term(1) = term(1) + vovo(a, l, a, j)
    term(2) = term(2) + oooo(j, l, i, i)
    term(3) = term(3) + oooo(i, l, i, j)

    term(0) = term(0) * (-1.9999999999999998d+0) 


    cisd22_aiajaial = 0.d+0
    do s = 0, 3
       cisd22_aiajaial = cisd22_aiajaial + term(s)
    end do

  end function cisd22_aiajaial
  function cisd22_aiajajal(a, i, j, l) 
    double precision :: cisd22_aiajajal   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, a, i, l)
    term(1) = term(1) + vovo(a, l, a, i)
    term(2) = term(2) + oooo(j, l, i, j)
    term(3) = term(3) + oooo(j, j, i, l)

    term(0) = term(0) * (-1.9999999999999998d+0) 


    cisd22_aiajajal = 0.d+0
    do s = 0, 3
       cisd22_aiajajal = cisd22_aiajajal + term(s)
    end do

  end function cisd22_aiajajal
  function cisd22_aibiakai(a, i, b, k) 
    double precision :: cisd22_aibiakai   
    integer, intent(in) :: a, i, b, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, b, i, k)
    term(1) = term(1) + vovo(b, i, a, k)

    term(0) = term(0) * (-1.9999999999999998d+0) 


    cisd22_aibiakai = 0.d+0
    do s = 0, 1
       cisd22_aibiakai = cisd22_aibiakai + term(s)
    end do

  end function cisd22_aibiakai
  function cisd22_aibjaiai(a, i, b, j) 
    double precision :: cisd22_aibjaiai   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, b, i, j)
    term(1) = term(1) + vovo(b, j, a, i)

    term(0) = term(0) * (-1.9999999999999998d+0) 
    term(1) = term(1) * 1.9999999999999998d+0 


    cisd22_aibjaiai = 0.d+0
    do s = 0, 1
       cisd22_aibjaiai = cisd22_aibjaiai + term(s)
    end do

  end function cisd22_aibjaiai
  function cisd22_aibjajai(a, i, b, j) 
    double precision :: cisd22_aibjajai   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) ! + vvvv(a, b, a, a)
    term(1) = term(1) + vvoo(a, b, i, i)
    term(2) = term(2) + vvoo(a, b, j, j)
    term(3) = term(3) + vovo(b, j, a, j)

    term(1) = -term(1) 
    term(2) = -term(2) 


    cisd22_aibjajai = 0.d+0
    do s = 0, 3
       cisd22_aibjajai = cisd22_aibjajai + term(s)
    end do

  end function cisd22_aibjajai
  function cisd22_aibjajaj(a, i, b, j) 
    double precision :: cisd22_aibjajaj   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, b, i, j)

    term(0) = term(0) * (-1.9999999999999998d+0) 


    cisd22_aibjajaj = 0.d+0
    do s = 0, 0
       cisd22_aibjajaj = cisd22_aibjajaj + term(s)
    end do

  end function cisd22_aibjajaj
  function cisd22_aibjaiaj(a, i, b, j) 
    double precision :: cisd22_aibjaiaj   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) ! + vvvv(a, b, a, a)
    term(1) = term(1) + vvoo(a, b, i, i)
    term(2) = term(2) + vvoo(a, b, j, j)
    term(3) = term(3) + vovo(b, j, a, j)

    term(1) = -term(1) 
    term(2) = -term(2) 


    cisd22_aibjaiaj = 0.d+0
    do s = 0, 3
       cisd22_aibjaiaj = cisd22_aibjaiaj + term(s)
    end do

  end function cisd22_aibjaiaj
  function cisd22_aibiaial(a, i, b, l) 
    double precision :: cisd22_aibiaial   
    integer, intent(in) :: a, i, b, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, b, i, l)
    term(1) = term(1) + vovo(b, i, a, l)

    term(0) = term(0) * (-1.9999999999999998d+0) 


    cisd22_aibiaial = 0.d+0
    do s = 0, 1
       cisd22_aibiaial = cisd22_aibiaial + term(s)
    end do

  end function cisd22_aibiaial
  function cisd22_aiaiakdi(a, i, k, d) 
    double precision :: cisd22_aiaiakdi   
    integer, intent(in) :: a, i, k, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, d, i, k)
    term(1) = term(1) + vovo(d, k, a, i)

    term(0) = -term(0) 
    term(1) = -term(1) 


    cisd22_aiaiakdi = 0.d+0
    do s = 0, 1
       cisd22_aiaiakdi = cisd22_aiaiakdi + term(s)
    end do

  end function cisd22_aiaiakdi
  function cisd22_aiajaidi(a, i, j, d) 
    double precision :: cisd22_aiajaidi   
    integer, intent(in) :: a, i, j, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, d, i, j)
    term(1) = term(1) + vovo(d, i, a, j)

    term(0) = term(0) * (-1.9999999999999998d+0) 


    cisd22_aiajaidi = 0.d+0
    do s = 0, 1
       cisd22_aiajaidi = cisd22_aiajaidi + term(s)
    end do

  end function cisd22_aiajaidi
  function cisd22_aiajajdi(a, i, j, d) 
    double precision :: cisd22_aiajajdi   
    integer, intent(in) :: a, i, j, d 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) ! + vvvv(a, d, a, a)
    term(1) = term(1) + vvoo(a, d, i, i)
    term(2) = term(2) + vvoo(a, d, j, j)
    term(3) = term(3) + vovo(d, j, a, j)
    term(4) = term(4) + vovo(d, i, a, i)

    term(1) = -term(1) 
    term(2) = -term(2) 
    term(3) = -term(3) 
    term(4) = term(4) * 1.9999999999999998d+0 


    cisd22_aiajajdi = 0.d+0
    do s = 0, 4
       cisd22_aiajajdi = cisd22_aiajajdi + term(s)
    end do

  end function cisd22_aiajajdi
  function cisd22_aiajajdj(a, i, j, d) 
    double precision :: cisd22_aiajajdj   
    integer, intent(in) :: a, i, j, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, d, i, j)
    term(1) = term(1) + vovo(d, j, a, i)

    term(0) = term(0) * (-1.9999999999999998d+0) 


    cisd22_aiajajdj = 0.d+0
    do s = 0, 1
       cisd22_aiajajdj = cisd22_aiajajdj + term(s)
    end do

  end function cisd22_aiajajdj
  function cisd22_aiajaidj(a, i, j, d) 
    double precision :: cisd22_aiajaidj   
    integer, intent(in) :: a, i, j, d 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) ! + vvvv(a, d, a, a)
    term(1) = term(1) + vvoo(a, d, i, i)
    term(2) = term(2) + vvoo(a, d, j, j)
    term(3) = term(3) + vovo(d, i, a, i)
    term(4) = term(4) + vovo(d, j, a, j)

    term(1) = -term(1) 
    term(2) = -term(2) 
    term(3) = -term(3) 
    term(4) = term(4) * 1.9999999999999998d+0 


    cisd22_aiajaidj = 0.d+0
    do s = 0, 4
       cisd22_aiajaidj = cisd22_aiajaidj + term(s)
    end do

  end function cisd22_aiajaidj
  function cisd22_aiaiaidl(a, i, d, l) 
    double precision :: cisd22_aiaiaidl   
    integer, intent(in) :: a, i, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, d, i, l)
    term(1) = term(1) + vovo(d, l, a, i)

    term(0) = -term(0) 
    term(1) = term(1) * 1.9999999999999996d+0 


    cisd22_aiaiaidl = 0.d+0
    do s = 0, 1
       cisd22_aiaiaidl = cisd22_aiaiaidl + term(s)
    end do

  end function cisd22_aiaiaidl
  function cisd22_aibiakbi(a, i, b, k) 
    double precision :: cisd22_aibiakbi   
    integer, intent(in) :: a, i, b, k 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, a, i, k)
    term(1) = term(1) + vvoo(b, b, i, k)
    term(2) = term(2) + vovo(a, k, a, i)
    term(3) = term(3) + vovo(b, k, b, i)
    term(4) = term(4) + oooo(i, k, i, i)

    term(0) = -term(0) 
    term(1) = -term(1) 
    term(2) = term(2) * 1.9999999999999998d+0 
    term(3) = -term(3) 


    cisd22_aibiakbi = 0.d+0
    do s = 0, 4
       cisd22_aibiakbi = cisd22_aibiakbi + term(s)
    end do

  end function cisd22_aibiakbi
  function cisd22_aibjaibi(a, i, b, j) 
    double precision :: cisd22_aibjaibi   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, a, i, j)
    term(1) = term(1) + vvoo(b, b, i, j)
    term(2) = term(2) + vovo(b, j, b, i)
    term(3) = term(3) + oooo(i, j, i, i)

    term(0) = -term(0) 
    term(1) = -term(1) 


    cisd22_aibjaibi = 0.d+0
    do s = 0, 3
       cisd22_aibjaibi = cisd22_aibjaibi + term(s)
    end do

  end function cisd22_aibjaibi
  function cisd22_aibjajbi(a, i, b, j) 
    double precision :: cisd22_aibjajbi   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + oooo(i, j, i, j)
    term(1) = term(1) + vovo(a, i, a, i)
    term(2) = term(2) + vovo(b, j, b, j)
    term(3) = term(3) ! + vvvv(a, b, a, b)

    term(1) = -term(1) 
    term(2) = -term(2) 


    cisd22_aibjajbi = 0.d+0
    do s = 0, 3
       cisd22_aibjajbi = cisd22_aibjajbi + term(s)
    end do

  end function cisd22_aibjajbi
  function cisd22_aibjajbj(a, i, b, j) 
    double precision :: cisd22_aibjajbj   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, a, i, j)
    term(1) = term(1) + vvoo(b, b, i, j)
    term(2) = term(2) + vovo(a, j, a, i)
    term(3) = term(3) + oooo(j, j, i, j)

    term(0) = -term(0) 
    term(1) = -term(1) 


    cisd22_aibjajbj = 0.d+0
    do s = 0, 3
       cisd22_aibjajbj = cisd22_aibjajbj + term(s)
    end do

  end function cisd22_aibjajbj
  function cisd22_aibjaibj(eorb, nocc, a, i, b, j) 
    real(F64), dimension(:), intent(in) ::eorb
    double precision :: cisd22_aibjaibj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,m,n 
    double precision, dimension(0:14) :: term 
    term = 0.d+0 
    term(0) = term(0) + oooo(j, j, i, i)
    term(1) = term(1) + eorb(a)
    term(2) = term(2) + eorb(b)
    term(3) = term(3) ! + vvvv(b, b, a, a)
    term(4) = term(4) + eorb(i)
    term(5) = term(5) + eorb(j)
    term(6) = term(6) + vvoo(a, a, i, i)
    term(7) = term(7) + vvoo(a, a, j, j)
    term(8) = term(8) + vvoo(b, b, i, i)
    term(9) = term(9) + vvoo(b, b, j, j)
    term(10) = term(10) + vovo(a, i, a, i)
    term(11) = term(11) + vovo(b, j, b, j)

    term(4) = -term(4) 
    term(5) = -term(5) 
    term(6) = -term(6) 
    term(7) = -term(7) 
    term(8) = -term(8) 
    term(9) = -term(9) 
    term(10) = term(10) * 1.9999999999999998d+0 
    term(11) = term(11) * 1.9999999999999998d+0 

    do m = 1, nocc 
       term(12) = term(12) + eorb(m)
    end do

    term(12) = term(12) * 2.0d+0 

    do m = 1, nocc 
       do n = 1, nocc 
          term(13) = term(13) + oooo(n, n, m, m)
       end do
    end do

    term(13) = term(13) * (-1.9999999999999998d+0) 

    do n = 1, nocc 
       do m = 1, nocc 
          term(14) = term(14) + oooo(m, n, m, n)
       end do
    end do



    cisd22_aibjaibj = 0.d+0
    do s = 0, 14
       cisd22_aibjaibj = cisd22_aibjaibj + term(s)
    end do

  end function cisd22_aibjaibj
  function cisd22_aibiakbk(i, k) 
    double precision :: cisd22_aibiakbk   
    integer, intent(in) :: i, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + oooo(i, k, i, k)



    cisd22_aibiakbk = 0.d+0
    do s = 0, 0
       cisd22_aibiakbk = cisd22_aibiakbk + term(s)
    end do

  end function cisd22_aibiakbk
  function cisd22_aibiaibl(a, i, b, l) 
    double precision :: cisd22_aibiaibl   
    integer, intent(in) :: a, i, b, l 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, a, i, l)
    term(1) = term(1) + vvoo(b, b, i, l)
    term(2) = term(2) + vovo(a, l, a, i)
    term(3) = term(3) + vovo(b, l, b, i)
    term(4) = term(4) + oooo(i, l, i, i)

    term(0) = -term(0) 
    term(1) = -term(1) 
    term(2) = -term(2) 
    term(3) = term(3) * 1.9999999999999998d+0 


    cisd22_aibiaibl = 0.d+0
    do s = 0, 4
       cisd22_aibiaibl = cisd22_aibiaibl + term(s)
    end do

  end function cisd22_aibiaibl
  function cisd22_aibiaidi(a, i, b, d) 
    double precision :: cisd22_aibiaidi   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) ! + vvvv(b, d, a, a)
    term(1) = term(1) ! + vvvv(a, d, a, b)
    term(2) = term(2) + vvoo(b, d, i, i)
    term(3) = term(3) + vovo(d, i, b, i)

    term(2) = term(2) * (-1.9999999999999998d+0) 


    cisd22_aibiaidi = 0.d+0
    do s = 0, 3
       cisd22_aibiaidi = cisd22_aibiaidi + term(s)
    end do

  end function cisd22_aibiaidi
  function cisd22_aibibkbi(a, i, b, k) 
    double precision :: cisd22_aibibkbi   
    integer, intent(in) :: a, i, b, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, b, i, k)
    term(1) = term(1) + vovo(b, k, a, i)

    term(0) = term(0) * (-1.9999999999999998d+0) 


    cisd22_aibibkbi = 0.d+0
    do s = 0, 1
       cisd22_aibibkbi = cisd22_aibibkbi + term(s)
    end do

  end function cisd22_aibibkbi
  function cisd22_aibjbibi(a, i, b, j) 
    double precision :: cisd22_aibjbibi   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, b, i, j)

    term(0) = term(0) * (-1.9999999999999998d+0) 


    cisd22_aibjbibi = 0.d+0
    do s = 0, 0
       cisd22_aibjbibi = cisd22_aibjbibi + term(s)
    end do

  end function cisd22_aibjbibi
  function cisd22_aibjbjbi(a, i, b, j) 
    double precision :: cisd22_aibjbjbi   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) ! + vvvv(b, b, a, b)
    term(1) = term(1) + vvoo(a, b, i, i)
    term(2) = term(2) + vvoo(a, b, j, j)
    term(3) = term(3) + vovo(b, i, a, i)

    term(1) = -term(1) 
    term(2) = -term(2) 


    cisd22_aibjbjbi = 0.d+0
    do s = 0, 3
       cisd22_aibjbjbi = cisd22_aibjbjbi + term(s)
    end do

  end function cisd22_aibjbjbi
  function cisd22_aibjbjbj(a, i, b, j) 
    double precision :: cisd22_aibjbjbj   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, b, i, j)
    term(1) = term(1) + vovo(b, j, a, i)

    term(0) = term(0) * (-1.9999999999999998d+0) 
    term(1) = term(1) * 1.9999999999999998d+0 


    cisd22_aibjbjbj = 0.d+0
    do s = 0, 1
       cisd22_aibjbjbj = cisd22_aibjbjbj + term(s)
    end do

  end function cisd22_aibjbjbj
  function cisd22_aibjbibj(a, i, b, j) 
    double precision :: cisd22_aibjbibj   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) ! + vvvv(b, b, a, b)
    term(1) = term(1) + vvoo(a, b, i, i)
    term(2) = term(2) + vvoo(a, b, j, j)
    term(3) = term(3) + vovo(b, i, a, i)

    term(1) = -term(1) 
    term(2) = -term(2) 


    cisd22_aibjbibj = 0.d+0
    do s = 0, 3
       cisd22_aibjbibj = cisd22_aibjbibj + term(s)
    end do

  end function cisd22_aibjbibj
  function cisd22_aibibibl(a, i, b, l) 
    double precision :: cisd22_aibibibl   
    integer, intent(in) :: a, i, b, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, b, i, l)
    term(1) = term(1) + vovo(b, l, a, i)

    term(0) = term(0) * (-1.9999999999999998d+0) 


    cisd22_aibibibl = 0.d+0
    do s = 0, 1
       cisd22_aibibibl = cisd22_aibibibl + term(s)
    end do

  end function cisd22_aibibibl
  function cisd22_aiajcicj(a, c) 
    double precision :: cisd22_aiajcicj   
    integer, intent(in) :: a, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) ! + vvvv(a, c, a, c)



    cisd22_aiajcicj = 0.d+0
    do s = 0, 0
       cisd22_aiajcicj = cisd22_aiajcicj + term(s)
    end do

  end function cisd22_aiajcicj
  function cisd22_aibicici(a, b, c) 
    double precision :: cisd22_aibicici   
    integer, intent(in) :: a, b, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) ! + vvvv(b, c, a, c)

    term(0) = term(0) * 1.9999999999999998d+0 


    cisd22_aibicici = 0.d+0
    do s = 0, 0
       cisd22_aibicici = cisd22_aibicici + term(s)
    end do

  end function cisd22_aibicici
  function cisd22_aibibidi(a, i, b, d) 
    double precision :: cisd22_aibibidi   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) ! + vvvv(b, d, a, b)
    term(1) = term(1) ! + vvvv(b, b, a, d)
    term(2) = term(2) + vvoo(a, d, i, i)
    term(3) = term(3) + vovo(d, i, a, i)

    term(2) = term(2) * (-1.9999999999999998d+0) 


    cisd22_aibibidi = 0.d+0
    do s = 0, 3
       cisd22_aibibidi = cisd22_aibibidi + term(s)
    end do

  end function cisd22_aibibidi
  function cisd22_aiaickai(a, i, c, k) 
    double precision :: cisd22_aiaickai   
    integer, intent(in) :: a, i, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, c, i, k)
    term(1) = term(1) + vovo(c, k, a, i)

    term(0) = -term(0) 
    term(1) = term(1) * 1.9999999999999996d+0 


    cisd22_aiaickai = 0.d+0
    do s = 0, 1
       cisd22_aiaickai = cisd22_aiaickai + term(s)
    end do

  end function cisd22_aiaickai
  function cisd22_aiajciai(a, i, j, c) 
    double precision :: cisd22_aiajciai   
    integer, intent(in) :: a, i, j, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, c, i, j)
    term(1) = term(1) + vovo(c, i, a, j)

    term(0) = term(0) * (-1.9999999999999998d+0) 


    cisd22_aiajciai = 0.d+0
    do s = 0, 1
       cisd22_aiajciai = cisd22_aiajciai + term(s)
    end do

  end function cisd22_aiajciai
  function cisd22_aiajcjai(a, i, j, c) 
    double precision :: cisd22_aiajcjai   
    integer, intent(in) :: a, i, j, c 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) ! + vvvv(a, c, a, a)
    term(1) = term(1) + vvoo(a, c, i, i)
    term(2) = term(2) + vvoo(a, c, j, j)
    term(3) = term(3) + vovo(c, i, a, i)
    term(4) = term(4) + vovo(c, j, a, j)

    term(1) = -term(1) 
    term(2) = -term(2) 
    term(3) = -term(3) 
    term(4) = term(4) * 1.9999999999999998d+0 


    cisd22_aiajcjai = 0.d+0
    do s = 0, 4
       cisd22_aiajcjai = cisd22_aiajcjai + term(s)
    end do

  end function cisd22_aiajcjai
  function cisd22_aiajcjaj(a, i, j, c) 
    double precision :: cisd22_aiajcjaj   
    integer, intent(in) :: a, i, j, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, c, i, j)
    term(1) = term(1) + vovo(c, j, a, i)

    term(0) = term(0) * (-1.9999999999999998d+0) 


    cisd22_aiajcjaj = 0.d+0
    do s = 0, 1
       cisd22_aiajcjaj = cisd22_aiajcjaj + term(s)
    end do

  end function cisd22_aiajcjaj
  function cisd22_aiajciaj(a, i, j, c) 
    double precision :: cisd22_aiajciaj   
    integer, intent(in) :: a, i, j, c 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) ! + vvvv(a, c, a, a)
    term(1) = term(1) + vvoo(a, c, i, i)
    term(2) = term(2) + vvoo(a, c, j, j)
    term(3) = term(3) + vovo(c, i, a, i)
    term(4) = term(4) + vovo(c, j, a, j)

    term(1) = -term(1) 
    term(2) = -term(2) 
    term(3) = term(3) * 1.9999999999999998d+0 
    term(4) = -term(4) 


    cisd22_aiajciaj = 0.d+0
    do s = 0, 4
       cisd22_aiajciaj = cisd22_aiajciaj + term(s)
    end do

  end function cisd22_aiajciaj
  function cisd22_aiaicial(a, i, c, l) 
    double precision :: cisd22_aiaicial   
    integer, intent(in) :: a, i, c, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, c, i, l)
    term(1) = term(1) + vovo(c, l, a, i)

    term(0) = -term(0) 
    term(1) = -term(1) 


    cisd22_aiaicial = 0.d+0
    do s = 0, 1
       cisd22_aiaicial = cisd22_aiaicial + term(s)
    end do

  end function cisd22_aiaicial
  function cisd22_aibiciai(a, i, b, c) 
    double precision :: cisd22_aibiciai   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) ! + vvvv(a, c, a, b)
    term(1) = term(1) ! + vvvv(b, c, a, a)
    term(2) = term(2) + vvoo(b, c, i, i)
    term(3) = term(3) + vovo(c, i, b, i)

    term(2) = term(2) * (-1.9999999999999998d+0) 


    cisd22_aibiciai = 0.d+0
    do s = 0, 3
       cisd22_aibiciai = cisd22_aibiciai + term(s)
    end do

  end function cisd22_aibiciai
  function cisd22_aiaicidi(a, c, d) 
    double precision :: cisd22_aiaicidi   
    integer, intent(in) :: a, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) ! + vvvv(a, d, a, c)



    cisd22_aiaicidi = 0.d+0
    do s = 0, 0
       cisd22_aiaicidi = cisd22_aiaicidi + term(s)
    end do

  end function cisd22_aiaicidi
  function cisd22_aibicibi(a, i, b, c) 
    double precision :: cisd22_aibicibi   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) ! + vvvv(b, b, a, c)
    term(1) = term(1) ! + vvvv(b, c, a, b)
    term(2) = term(2) + vvoo(a, c, i, i)
    term(3) = term(3) + vovo(c, i, a, i)

    term(2) = term(2) * (-1.9999999999999998d+0) 


    cisd22_aibicibi = 0.d+0
    do s = 0, 3
       cisd22_aibicibi = cisd22_aibicibi + term(s)
    end do

  end function cisd22_aibicibi
  function cisd22_aiaiakai(a, i, k) 
    double precision :: cisd22_aiaiakai   
    integer, intent(in) :: a, i, k 
    integer :: s  
    double precision, dimension(0:2) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, a, i, k)
    term(1) = term(1) + vovo(a, k, a, i)
    term(2) = term(2) + oooo(i, k, i, i)

    term(0) = term(0) * (-1.9999999999999996d+0) 


    cisd22_aiaiakai = 0.d+0
    do s = 0, 2
       cisd22_aiaiakai = cisd22_aiaiakai + term(s)
    end do

  end function cisd22_aiaiakai
  function cisd22_aiajaiai(a, i, j) 
    double precision :: cisd22_aiajaiai   
    integer, intent(in) :: a, i, j 
    integer :: s  
    double precision, dimension(0:2) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, a, i, j)
    term(1) = term(1) + vovo(a, j, a, i)
    term(2) = term(2) + oooo(i, j, i, i)

    term(0) = term(0) * (-3.9999999999999996d+0) 
    term(1) = term(1) * 1.9999999999999998d+0 
    term(2) = term(2) * 1.9999999999999998d+0 


    cisd22_aiajaiai = 0.d+0
    do s = 0, 2
       cisd22_aiajaiai = cisd22_aiajaiai + term(s)
    end do

  end function cisd22_aiajaiai
  function cisd22_aiajajaj(a, i, j) 
    double precision :: cisd22_aiajajaj   
    integer, intent(in) :: a, i, j 
    integer :: s  
    double precision, dimension(0:2) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, a, i, j)
    term(1) = term(1) + vovo(a, j, a, i)
    term(2) = term(2) + oooo(j, j, i, j)

    term(0) = term(0) * (-3.9999999999999996d+0) 
    term(1) = term(1) * 1.9999999999999998d+0 
    term(2) = term(2) * 1.9999999999999998d+0 


    cisd22_aiajajaj = 0.d+0
    do s = 0, 2
       cisd22_aiajajaj = cisd22_aiajajaj + term(s)
    end do

  end function cisd22_aiajajaj
  function cisd22_aiajaiaj(eorb, nocc, a, i, j) 
    real(F64), dimension(:), intent(in) ::eorb
    double precision :: cisd22_aiajaiaj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, j 
    integer :: s ,m,n 
    double precision, dimension(0:12) :: term 
    term = 0.d+0 
    term(0) = term(0) + oooo(j, j, i, i)
    term(1) = term(1) + oooo(i, j, i, j)
    term(2) = term(2) ! + vvvv(a, a, a, a)
    term(3) = term(3) + eorb(a)
    term(4) = term(4) + eorb(i)
    term(5) = term(5) + eorb(j)
    term(6) = term(6) + vvoo(a, a, i, i)
    term(7) = term(7) + vvoo(a, a, j, j)
    term(8) = term(8) + vovo(a, i, a, i)
    term(9) = term(9) + vovo(a, j, a, j)

    term(3) = term(3) * 2.0d+0 
    term(4) = -term(4) 
    term(5) = -term(5) 
    term(6) = term(6) * (-1.9999999999999998d+0) 
    term(7) = term(7) * (-1.9999999999999998d+0) 

    do n = 1, nocc 
       do m = 1, nocc 
          term(10) = term(10) + oooo(m, n, m, n)
       end do
    end do


    do m = 1, nocc 
       do n = 1, nocc 
          term(11) = term(11) + oooo(n, n, m, m)
       end do
    end do

    term(11) = term(11) * (-1.9999999999999998d+0) 

    do m = 1, nocc 
       term(12) = term(12) + eorb(m)
    end do

    term(12) = term(12) * 2.0d+0 


    cisd22_aiajaiaj = 0.d+0
    do s = 0, 12
       cisd22_aiajaiaj = cisd22_aiajaiaj + term(s)
    end do

  end function cisd22_aiajaiaj
  function cisd22_aiaiakak(i, k) 
    double precision :: cisd22_aiaiakak   
    integer, intent(in) :: i, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + oooo(i, k, i, k)



    cisd22_aiaiakak = 0.d+0
    do s = 0, 0
       cisd22_aiaiakak = cisd22_aiaiakak + term(s)
    end do

  end function cisd22_aiaiakak
  function cisd22_aiaiaial(a, i, l) 
    double precision :: cisd22_aiaiaial   
    integer, intent(in) :: a, i, l 
    integer :: s  
    double precision, dimension(0:2) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvoo(a, a, i, l)
    term(1) = term(1) + vovo(a, l, a, i)
    term(2) = term(2) + oooo(i, l, i, i)

    term(0) = term(0) * (-1.9999999999999996d+0) 


    cisd22_aiaiaial = 0.d+0
    do s = 0, 2
       cisd22_aiaiaial = cisd22_aiaiaial + term(s)
    end do

  end function cisd22_aiaiaial
  function cisd22_aibiaiai(a, i, b) 
    double precision :: cisd22_aibiaiai   
    integer, intent(in) :: a, i, b 
    integer :: s  
    double precision, dimension(0:2) :: term 
    term = 0.d+0 
    term(0) = term(0) ! + vvvv(a, b, a, a)
    term(1) = term(1) + vvoo(a, b, i, i)
    term(2) = term(2) + vovo(b, i, a, i)

    term(0) = term(0) * 1.9999999999999998d+0 
    term(1) = term(1) * (-3.9999999999999996d+0) 
    term(2) = term(2) * 1.9999999999999998d+0 


    cisd22_aibiaiai = 0.d+0
    do s = 0, 2
       cisd22_aibiaiai = cisd22_aibiaiai + term(s)
    end do

  end function cisd22_aibiaiai
  function cisd22_aiaiaidi(a, i, d) 
    double precision :: cisd22_aiaiaidi   
    integer, intent(in) :: a, i, d 
    integer :: s  
    double precision, dimension(0:2) :: term 
    term = 0.d+0 
    term(0) = term(0) ! + vvvv(a, d, a, a)
    term(1) = term(1) + vvoo(a, d, i, i)
    term(2) = term(2) + vovo(d, i, a, i)

    term(1) = term(1) * (-1.9999999999999996d+0) 


    cisd22_aiaiaidi = 0.d+0
    do s = 0, 2
       cisd22_aiaiaidi = cisd22_aiaiaidi + term(s)
    end do

  end function cisd22_aiaiaidi
  function cisd22_aibiaibi(eorb, nocc, a, i, b) 
    real(F64), dimension(:), intent(in) ::eorb
    double precision :: cisd22_aibiaibi 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b 
    integer :: s ,m,n 
    double precision, dimension(0:12) :: term 
    term = 0.d+0 
    term(0) = term(0) + eorb(a)
    term(1) = term(1) + eorb(b)
    term(2) = term(2) ! + vvvv(b, b, a, a)
    term(3) = term(3) ! + vvvv(a, b, a, b)
    term(4) = term(4) + oooo(i, i, i, i)
    term(5) = term(5) + eorb(i)
    term(6) = term(6) + vvoo(a, a, i, i)
    term(7) = term(7) + vvoo(b, b, i, i)
    term(8) = term(8) + vovo(a, i, a, i)
    term(9) = term(9) + vovo(b, i, b, i)

    term(5) = term(5) * (-2.0d+0) 
    term(6) = term(6) * (-1.9999999999999998d+0) 
    term(7) = term(7) * (-1.9999999999999998d+0) 

    do n = 1, nocc 
       do m = 1, nocc 
          term(10) = term(10) + oooo(m, n, m, n)
       end do
    end do


    do m = 1, nocc 
       do n = 1, nocc 
          term(11) = term(11) + oooo(n, n, m, m)
       end do
    end do

    term(11) = term(11) * (-1.9999999999999998d+0) 

    do m = 1, nocc 
       term(12) = term(12) + eorb(m)
    end do

    term(12) = term(12) * 2.0d+0 


    cisd22_aibiaibi = 0.d+0
    do s = 0, 12
       cisd22_aibiaibi = cisd22_aibiaibi + term(s)
    end do

  end function cisd22_aibiaibi
  function cisd22_aibibibi(a, i, b) 
    double precision :: cisd22_aibibibi   
    integer, intent(in) :: a, i, b 
    integer :: s  
    double precision, dimension(0:2) :: term 
    term = 0.d+0 
    term(0) = term(0) ! + vvvv(b, b, a, b)
    term(1) = term(1) + vvoo(a, b, i, i)
    term(2) = term(2) + vovo(b, i, a, i)

    term(0) = term(0) * 1.9999999999999998d+0 
    term(1) = term(1) * (-3.9999999999999996d+0) 
    term(2) = term(2) * 1.9999999999999998d+0 


    cisd22_aibibibi = 0.d+0
    do s = 0, 2
       cisd22_aibibibi = cisd22_aibibibi + term(s)
    end do

  end function cisd22_aibibibi
  function cisd22_aiaicici(a, c) 
    double precision :: cisd22_aiaicici   
    integer, intent(in) :: a, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) ! + vvvv(a, c, a, c)



    cisd22_aiaicici = 0.d+0
    do s = 0, 0
       cisd22_aiaicici = cisd22_aiaicici + term(s)
    end do

  end function cisd22_aiaicici
  function cisd22_aiaiciai(a, i, c) 
    double precision :: cisd22_aiaiciai   
    integer, intent(in) :: a, i, c 
    integer :: s  
    double precision, dimension(0:2) :: term 
    term = 0.d+0 
    term(0) = term(0) ! + vvvv(a, c, a, a)
    term(1) = term(1) + vvoo(a, c, i, i)
    term(2) = term(2) + vovo(c, i, a, i)

    term(1) = term(1) * (-1.9999999999999996d+0) 


    cisd22_aiaiciai = 0.d+0
    do s = 0, 2
       cisd22_aiaiciai = cisd22_aiaiciai + term(s)
    end do

  end function cisd22_aiaiciai
  function cisd22_aiaiaiai(eorb, nocc, a, i) 
    real(F64), dimension(:), intent(in) ::eorb
    double precision :: cisd22_aiaiaiai 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i 
    integer :: s ,m,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + oooo(i, i, i, i)
    term(1) = term(1) + eorb(i)
    term(2) = term(2) + vvoo(a, a, i, i)
    term(3) = term(3) + vovo(a, i, a, i)
    term(4) = term(4) ! + vvvv(a, a, a, a)
    term(5) = term(5) + eorb(a)

    term(1) = term(1) * (-2.0d+0) 
    term(2) = term(2) * (-3.999999999999999d+0) 
    term(3) = term(3) * 1.9999999999999991d+0 
    term(5) = term(5) * 2.0d+0 

    do m = 1, nocc 
       term(6) = term(6) + eorb(m)
    end do

    term(6) = term(6) * 2.0d+0 

    do m = 1, nocc 
       do n = 1, nocc 
          term(7) = term(7) + oooo(n, n, m, m)
       end do
    end do

    term(7) = term(7) * (-1.9999999999999996d+0) 

    do n = 1, nocc 
       do m = 1, nocc 
          term(8) = term(8) + oooo(m, n, m, n)
       end do
    end do



    cisd22_aiaiaiai = 0.d+0
    do s = 0, 8
       cisd22_aiaiaiai = cisd22_aiaiaiai + term(s)
    end do

  end function cisd22_aiaiaiai
end module cisd22

