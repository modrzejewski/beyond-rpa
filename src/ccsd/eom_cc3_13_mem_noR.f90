module eom_cc3_13_mem_noR

    use ccsd_transformed_integrals
    use t1_transformed_int
    use basis
    use arithmetic
    use cc3_intermediates
    use cc_gparams
    implicit none
    !               
    ! File generated automatically on 2017-04-20 12:32:54  
    !  
    contains
                                                                                                         
        subroutine sub_eom_cc3_13_mem_noR_z0(b, j, c, k, d, l, sigrows, d_small, sigma, vdav, ntrial, nocc0, nvirt0, nocc, n0a,n1a,n0i,n1i, eorb, wr)
        real(F64), dimension(:, :), intent(inout) :: sigrows
        real(F64), dimension(:,:), intent(inout) :: d_small
        real(F64), dimension(:), intent(in) :: sigma
        real(F64), intent(in) :: eorb, wr
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0, nocc
        real(F64) :: tmp
    integer, intent(in) :: n0a,n1a,n0i,n1i                                              
            integer, intent(in) :: ntrial, b, j, c, k, d, l
            integer :: p1, p2, ai, bj, ibra 
        integer :: hh
        hh = 1

            
    if (k >= n0i .and. k <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
ibra = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial
hh = hh + 1 

tmp = (one/(eorb-wr)) * sigma(p1) * vovo(b, l, c, j)
do p2 = 1, ntrial
        d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 

end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
ibra = (d - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial
 

tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * vovo(b, k, c, j)
do p2 = 1, ntrial
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
end do
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp
end do

end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
ibra = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial
 

tmp = (one/(eorb-wr)) * sigma(p1) * vovo(b, k, d, j)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
ibra = (c - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial
 

tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * vovo(b, l, d, j)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        

!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (j >= n0i .and. j <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
ibra = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial
 

tmp = (one/(eorb-wr)) * sigma(p1) * vovo(b, k, c, l)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        

!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (j >= n0i .and. j <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
ibra = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial
 

tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * vovo(b, l, c, k)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        

!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
ibra = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial
 

tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * vovo(b, j, c, l)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        

!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
ibra = (d - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial
 

tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * vovo(b, j, c, k)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        

!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
ibra = (c - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial
 

tmp = (one/(eorb-wr)) * sigma(p1) * vovo(b, l, d, k)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        

!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
ibra = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial
 

tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * vovo(b, j, d, k)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        

!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
ibra = (c - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial
 

tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * vovo(b, k, d, l)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        

!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
ibra = (c - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial
 

tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * vovo(b, j, d, l)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        

!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (b >= n0a .and. b <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
ibra = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial
 

tmp = (one/(eorb-wr)) * sigma(p1) * vovo(c, j, d, k)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        

!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (b >= n0a .and. b <= n1a) then 
ibra = (b - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial
 

tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * vovo(c, j, d, l)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        

!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (b >= n0a .and. b <= n1a) then 
ibra = (b - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial
 

tmp = (one/(eorb-wr)) * sigma(p1) * vovo(c, l, d, j)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        

!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (b >= n0a .and. b <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
ibra = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial
 

tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * vovo(c, k, d, j)
do p2 = 1, ntrial
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
        end do
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (b >= n0a .and. b <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
ibra = (b - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial
 

tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * vovo(c, l, d, k)
do p2 = 1, ntrial
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
        end do
        

!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (b >= n0a .and. b <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
ibra = (b - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial
 

tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * vovo(c, k, d, l)
do p2 = 1, ntrial
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
        end do
       
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

!print*, 'hh z z0 z 13_mem_noR', hh

    end subroutine sub_eom_cc3_13_mem_noR_z0                                                                                                     
        subroutine sub_eom_cc3_13_mem_noR_z1(b, j, c, k, d, sigrows, d_small, sigma, vdav, ntrial, nocc0, nvirt0, nocc, n0a,n1a,n0i,n1i, eorb, wr)
        real(F64), dimension(:,:), intent(inout) :: d_small, sigrows
        real(F64), dimension(:), intent(in) :: sigma
        real(F64), intent(in) :: eorb, wr
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0, nocc
        real(F64) :: tmp
        integer, intent(in) :: n0a,n1a,n0i,n1i                                              
        integer, intent(in) :: ntrial, b, j, c, k, d
        integer :: p1, p2, ai, bj, ibra 
        integer :: hh
        hh = 1

            
    if (k >= n0i .and. k <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
ibra = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial
 

tmp =  (one/(eorb-wr)) * (- sigma(p1)) * vovo(b, j, c, j)
!!if(ibra==11)!!print*,'ibraibraibra', ibra, 1
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
       
 !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 

end if 
end if 

if (j >= n0i .and. j <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
ibra = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial
 

tmp =  (one/(eorb-wr)) * (- sigma(p1)) * vovo(b, k, c, j)
!!if(ibra==11)!!print*,'ibraibraibra', ibra, 2
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
       
 !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 

end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
ibra = (c - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial
 

tmp =  (one/(eorb-wr)) * (- sigma(p1)) * vovo(b, k, d, j)
!!if(ibra==11)!!print*,'ibraibraibra', ibra, 3
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
       
 !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 

end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
ibra = (c - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial
 

tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * vovo(b, j, d, j)
!!if(ibra==11)!!print*,'ibraibraibra', ibra, 4
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
       
 !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 

end if 
end if 

if (j >= n0i .and. j <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
ibra = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial
 

tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * vovo(b, j, c, k)
!!if(ibra==11)!!print*,'ibraibraibra', ibra, 5
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
       
 !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 

end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
ibra = (c - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial
 

tmp =  (one/(eorb-wr)) * (- sigma(p1)) * vovo(b, j, d, k)
!!if(ibra==11)!!print*,'ibraibraibra', ibra, 6
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
       
 !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 

end if 
end if 

if (b >= n0a .and. b <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
ibra = (b - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial
 

tmp =  (one/(eorb-wr)) * (- sigma(p1)) * vovo(c, j, d, k)
!!if(ibra==11)!!print*,'ibraibraibra', ibra, 7, -vovo(c, j, d, k)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
       
 !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 

end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (b >= n0a .and. b <= n1a) then 
ibra = (b - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial
 

tmp =  (one/(eorb-wr)) * (- sigma(p1)) * vovo(c, j, d, j)
!!if(ibra==11)!!print*,'ibraibraibra', ibra, 8
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
       
 !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 

end if 
end if 

if (b >= n0a .and. b <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
ibra = (b - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial
 

tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * vovo(c, k, d, j)
!!if(ibra==11)!!print*,'ibraibraibra', ibra, 9, 2.d+0 * vovo(c, k, d, j)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
       
 !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 

end if 
end if 

!print*, 'hh z z1 z 13_mem_noR', hh
    end subroutine sub_eom_cc3_13_mem_noR_z1                                                                                                     
        subroutine sub_eom_cc3_13_mem_noR_z2(b, j, c, k, d, sigrows, d_small, sigma, vdav, ntrial, nocc0, nvirt0, nocc, n0a,n1a,n0i,n1i, eorb, wr)
        real(F64), dimension(:, :), intent(inout) :: sigrows
        real(F64), dimension(:,:), intent(inout) :: d_small
        real(F64), dimension(:), intent(in) :: sigma
        real(F64), intent(in) :: eorb, wr
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0, nocc
        real(F64) :: tmp
    integer, intent(in) :: n0a,n1a,n0i,n1i                                              
            integer, intent(in) :: ntrial, b, j, c, k, d
            integer :: p1, p2, ai, bj, ibra 
        integer :: hh
        hh = 1

            
    if (k >= n0i .and. k <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
ibra = (d - nvirt0) * nocc + (k - nocc0) + 1 
!if(ibra==33.and.abs(sigma(2)).gt.1.d-3)!!print*,'ibraibraibra', ibra, 11, sigma(2)
do p1 = 1, ntrial

tmp =  (one/(eorb-wr)) * (- sigma(p1)) * vovo(b, k, c, j)
!!if(ibra==33)!!print*,'ibraibraibra', ibra, 10
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-3.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-3)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
ibra = (c - nvirt0) * nocc + (k - nocc0) + 1 
!if(ibra==33.and.abs(sigma(2)).gt.1.d-3)!!print*,'ibraibraibra', ibra, 12, sigma(2), b, j, c, k, d
do p1 = 1, ntrial

tmp =  (one/(eorb-wr)) * (- sigma(p1)) * vovo(b, k, d, j)
!!if(ibra==33)!!print*,'ibraibraibra', ibra, 11
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-3.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-3)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
!        if (p1==2.and.ibra==33)!!print*, sigrows(p1, ibra)
         end do 

end if 
end if 

if (j >= n0i .and. j <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
ibra = (d - nvirt0) * nocc + (j - nocc0) + 1 
!if(ibra==33.and.abs(sigma(2)).gt.1.d-3)!!print*,'ibraibraibra', ibra, 13, sigma(2)
do p1 = 1, ntrial

tmp =  (one/(eorb-wr)) * (- sigma(p1)) * vovo(b, k, c, k)
!!if(ibra==33)!!print*,'ibraibraibra', ibra, 12
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-3.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-3)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
ibra = (d - nvirt0) * nocc + (k - nocc0) + 1 
!if(ibra==33.and.abs(sigma(2)).gt.1.d-3)!!print*,'ibraibraibra', ibra, 14, sigma(2)
do p1 = 1, ntrial

tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * vovo(b, j, c, k)
!!if(ibra==33)!!print*,'ibraibraibra', ibra, 13
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-3.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-3)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
ibra = (c - nvirt0) * nocc + (j - nocc0) + 1 
!if(ibra==33.and.abs(sigma(2)).gt.1.d-3)!!print*,'ibraibraibra', ibra, 15, sigma(2)
do p1 = 1, ntrial

tmp =  (one/(eorb-wr)) * (- sigma(p1)) * vovo(b, k, d, k)
!!if(ibra==33)!!print*,'ibraibraibra', ibra, 14
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-3.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-3)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
ibra = (c - nvirt0) * nocc + (k - nocc0) + 1 
!if(ibra==33.and.abs(sigma(2)).gt.1.d-3)!!print*,'ibraibraibra', ibra, 20, sigma(2)
do p1 = 1, ntrial

tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * vovo(b, j, d, k)
!!if(ibra==33)!!print*,'ibraibraibra', ibra, 15
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-3.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-3)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (b >= n0a .and. b <= n1a) then 
ibra = (b - nvirt0) * nocc + (k - nocc0) + 1 
!if(ibra==33.and.abs(sigma(2)).gt.1.d-3)!!print*,'ibraibraibra', ibra, 16, sigma(2)
do p1 = 1, ntrial

tmp =  (one/(eorb-wr)) * (- sigma(p1)) * vovo(c, j, d, k)
!!if(ibra==33)!!print*,'ibraibraibra', ibra, 16

do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-3.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-3)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (b >= n0a .and. b <= n1a) then 
ibra = (b - nvirt0) * nocc + (k - nocc0) + 1 
!if(ibra==33.and.abs(sigma(2)).gt.1.d-3)!!print*,'ibraibraibra', ibra, 17, sigma(2)
do p1 = 1, ntrial

tmp =  (one/(eorb-wr)) * (- sigma(p1)) * vovo(c, k, d, j)
!!if(ibra==33)!!print*,'ibraibraibra', ibra, 17
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-3.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-3)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (b >= n0a .and. b <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
ibra = (b - nvirt0) * nocc + (j - nocc0) + 1 
!if(ibra==33.and.abs(sigma(2)).gt.1.d-3)!!print*,'ibraibraibra', ibra, 18, sigma(2)
do p1 = 1, ntrial

tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * vovo(c, k, d, k)
!!if(ibra==33)!!print*,'ibraibraibra', ibra, 18, 2.d+0 * vovo(c, k, d, k)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-3.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

!print*, 'hh z z2 z 13_mem_noR', hh
    end subroutine sub_eom_cc3_13_mem_noR_z2                                                                                                     
        subroutine sub_eom_cc3_13_mem_noR_z34(b, j, k, d, l, sigrows, d_small, sigma, vdav, ntrial, nocc0, nvirt0, nocc, n0a,n1a,n0i,n1i, eorb, wr)
        real(F64), dimension(:, :), intent(inout) :: sigrows
        real(F64), dimension(:,:), intent(inout) :: d_small
        real(F64), dimension(:), intent(in) :: sigma
        real(F64), intent(in) :: eorb, wr
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0, nocc
        real(F64) :: tmp
    integer, intent(in) :: n0a,n1a,n0i,n1i                                              
            integer, intent(in) :: ntrial, b, j, k, d, l
            integer :: p1, p2, ai, bj, ibra 
        integer :: hh
        hh = 1

            
    if (k >= n0i .and. k <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
ibra = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial


tmp =  (one/(eorb-wr)) * (- sigma(p1)) * vovo(b, j, b, l)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
ibra = (d - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial


tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * vovo(b, j, b, k)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (b >= n0a .and. b <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
ibra = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial
hh = hh + 1 

tmp =  (one/(eorb-wr)) * (- sigma(p1)) * vovo(b, k, d, j)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (b >= n0a .and. b <= n1a) then 
ibra = (b - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial
hh = hh + 1 

tmp =  (one/(eorb-wr)) * (- sigma(p1)) * vovo(b, l, d, j)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (j >= n0i .and. j <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
ibra = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial
hh = hh + 1 

tmp =  (one/(eorb-wr)) * (- sigma(p1)) * vovo(b, k, b, l)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (b >= n0a .and. b <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
ibra = (b - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial
hh = hh + 1 

tmp =  (one/(eorb-wr)) * (- sigma(p1)) * vovo(b, l, d, k)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (b >= n0a .and. b <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
ibra = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial
hh = hh + 1 

tmp =  (one/(eorb-wr)) * (- sigma(p1)) * vovo(b, j, d, k)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (b >= n0a .and. b <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
ibra = (b - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial
hh = hh + 1 

tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * vovo(b, k, d, l)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (b >= n0a .and. b <= n1a) then 
ibra = (b - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial
hh = hh + 1 

tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * vovo(b, j, d, l)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

!print*, 'hh z z34 z 13_mem_noR', hh
    end subroutine sub_eom_cc3_13_mem_noR_z34                                                                                                     
        subroutine sub_eom_cc3_13_mem_noR_z56(b, j, c, k, l, sigrows, d_small, sigma, vdav, ntrial, nocc0, nvirt0, nocc, n0a,n1a,n0i,n1i, eorb, wr)
        real(F64), dimension(:, :), intent(inout) :: sigrows
        real(F64), dimension(:,:), intent(inout) :: d_small
        real(F64), dimension(:), intent(in) :: sigma
        real(F64), intent(in) :: eorb, wr
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0, nocc
        real(F64) :: tmp
    integer, intent(in) :: n0a,n1a,n0i,n1i                                              
            integer, intent(in) :: ntrial, b, j, c, k, l
            integer :: p1, p2, ai, bj, ibra 
        integer :: hh
        hh = 1

            
    if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
ibra = (c - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial
hh = hh + 1 

tmp =  (one/(eorb-wr)) * (- sigma(p1)) * vovo(b, l, c, j)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
ibra = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial
hh = hh + 1 

tmp =  (one/(eorb-wr)) * (- sigma(p1)) * vovo(b, k, c, j)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
ibra = (c - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial
hh = hh + 1 

tmp =  (one/(eorb-wr)) * (- sigma(p1)) * vovo(b, k, c, l)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
ibra = (c - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial
hh = hh + 1 

tmp =  (one/(eorb-wr)) * (- sigma(p1)) * vovo(b, l, c, k)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
ibra = (c - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial
hh = hh + 1 

tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * vovo(b, j, c, l)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
ibra = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial
hh = hh + 1 

tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * vovo(b, j, c, k)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (b >= n0a .and. b <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
ibra = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial
hh = hh + 1 

tmp =  (one/(eorb-wr)) * (- sigma(p1)) * vovo(c, j, c, k)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (b >= n0a .and. b <= n1a) then 
ibra = (b - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial
hh = hh + 1 

tmp =  (one/(eorb-wr)) * (- sigma(p1)) * vovo(c, j, c, l)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (b >= n0a .and. b <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
ibra = (b - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial
hh = hh + 1 

tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * vovo(c, k, c, l)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

!print*, 'hh z z56 z 13_mem_noR', hh
    end subroutine sub_eom_cc3_13_mem_noR_z56                                                                                                     
        subroutine sub_eom_cc3_13_mem_noR_z7(b, j, k, d, sigrows, d_small, sigma, vdav, ntrial, nocc0, nvirt0, nocc, n0a,n1a,n0i,n1i, eorb, wr)
        real(F64), dimension(:, :), intent(inout) :: sigrows
        real(F64), dimension(:,:), intent(inout) :: d_small
        real(F64), dimension(:), intent(in) :: sigma
        real(F64), intent(in) :: eorb, wr
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0, nocc
        real(F64) :: tmp
    integer, intent(in) :: n0a,n1a,n0i,n1i                                              
            integer, intent(in) :: ntrial, b, j, k, d
            integer :: p1, p2, ai, bj, ibra 
                    integer :: hh
        hh = 1

    if (k >= n0i .and. k <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
ibra = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial
hh = hh + 1 

tmp =  (one/(eorb-wr)) * (- sigma(p1)) * vovo(b, j, b, j)
!!if(ibra==33)!!print*,'ibraibraibra', ibra, 19
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (j >= n0i .and. j <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
ibra = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial
hh = hh + 1 

tmp = (one/(eorb-wr)) * sigma(p1) * vovo(b, j, b, k)
!!if(ibra==33)!!print*,'ibraibraibra', ibra, 20
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (b >= n0a .and. b <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
ibra = (b - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial
hh = hh + 1 

tmp = (one/(eorb-wr)) * sigma(p1) * vovo(b, k, d, j)
!!if(ibra==33)!!print*,'ibraibraibra', ibra, 21
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (b >= n0a .and. b <= n1a) then 
ibra = (b - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial
hh = hh + 1 

tmp = (one/(eorb-wr)) * sigma(p1) * vovo(b, j, d, j)
!!if(ibra==33)!!print*,'ibraibraibra', ibra, 22
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (b >= n0a .and. b <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
ibra = (b - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial
hh = hh + 1 

tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * vovo(b, j, d, k)
!!if(ibra==33)!!print*,'ibraibraibra', ibra, 23
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

!print*, 'hh z z8 z 13_mem_noR', hh
    end subroutine sub_eom_cc3_13_mem_noR_z7                                                                                                     
        subroutine sub_eom_cc3_13_mem_noR_z8(b, j, k, d, sigrows, d_small, sigma, vdav, ntrial, nocc0, nvirt0, nocc, n0a,n1a,n0i,n1i, eorb, wr)
        real(F64), dimension(:, :), intent(inout) :: sigrows
        real(F64), dimension(:,:), intent(inout) :: d_small
        real(F64), dimension(:), intent(in) :: sigma
        real(F64), intent(in) :: eorb, wr
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0, nocc
        real(F64) :: tmp
    integer, intent(in) :: n0a,n1a,n0i,n1i                                              
            integer, intent(in) :: ntrial, b, j, k, d
            integer :: p1, p2, ai, bj, ibra 
        integer :: hh
        hh = 1

            
    if (k >= n0i .and. k <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
ibra = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial
hh = hh + 1 

tmp = (one/(eorb-wr)) * sigma(p1) * vovo(b, j, b, k)
!!if(ibra==33)!!print*,'ibraibraibra', ibra, 24
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (b >= n0a .and. b <= n1a) then 
ibra = (b - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial
hh = hh + 1 

tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * vovo(b, k, d, j)
!!if(ibra==33)!!print*,'ibraibraibra', ibra, 25
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (j >= n0i .and. j <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
ibra = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial
hh = hh + 1 

tmp =  (one/(eorb-wr)) * (- sigma(p1)) * vovo(b, k, b, k)
!!if(ibra==33)!!print*,'ibraibraibra', ibra, 26
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (b >= n0a .and. b <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
ibra = (b - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial
hh = hh + 1 

tmp = (one/(eorb-wr)) * sigma(p1) * vovo(b, k, d, k)
!!if(ibra==33)!!print*,'ibraibraibra', ibra, 27
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (b >= n0a .and. b <= n1a) then 
ibra = (b - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial
hh = hh + 1 

tmp = (one/(eorb-wr)) * sigma(p1) * vovo(b, j, d, k)
!!if(ibra==33)!!print*,'ibraibraibra', ibra, 28
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

!print*, 'hh z z8 z 13_mem_noR', hh
    end subroutine sub_eom_cc3_13_mem_noR_z8                                                                                                     
        subroutine sub_eom_cc3_13_mem_noR_z9(b, j, c, k, sigrows, d_small, sigma, vdav, ntrial, nocc0, nvirt0, nocc, n0a,n1a,n0i,n1i, eorb, wr)
        real(F64), dimension(:, :), intent(inout) :: sigrows
        real(F64), dimension(:,:), intent(inout) :: d_small
        real(F64), dimension(:), intent(in) :: sigma
        real(F64), intent(in) :: eorb, wr
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0, nocc
        real(F64) :: tmp
    integer, intent(in) :: n0a,n1a,n0i,n1i                                              
            integer, intent(in) :: ntrial, b, j, c, k
            integer :: p1, p2, ai, bj, ibra 
        integer :: hh
        hh = 1

            
    if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
ibra = (c - nvirt0) * nocc + (k - nocc0) + 1 

do p1 = 1, ntrial
cc_hh(9) = cc_hh(9) + 1 

tmp = (one/(eorb-wr)) * sigma(p1) * vovo(b, j, c, j)
!!!if(ibra==33)!!print*,'1ibraibraibra', ibra,  vovo(b, j, c, j)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
ibra = (c - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial
cc_hh(9) = cc_hh(9) + 1 

tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * vovo(b, k, c, j)
!!!if(ibra==11)!!print*,'2ibraibraibra', ibra,  -2.d+0*vovo(b, k, c, j)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
ibra = (c - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial
cc_hh(9) = cc_hh(9) + 1 

tmp = (one/(eorb-wr)) * sigma(p1) * vovo(b, j, c, k)
!!!if(ibra==11)!!print*,'3ibraibraibra', ibra,  vovo(b, j, c, k)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (b >= n0a .and. b <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
ibra = (b - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial
cc_hh(9) = cc_hh(9) + 1 

tmp = (one/(eorb-wr)) * sigma(p1) * vovo(c, j, c, k)
!!!if(ibra==11)!!print*,'4ibraibraibra', ibra,  p1, vovo(c, j, c, k), (one/(eorb-wr)), sigma(p1)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (b >= n0a .and. b <= n1a) then 
ibra = (b - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial
cc_hh(9) = cc_hh(9) + 1 

tmp =  (one/(eorb-wr)) * (- sigma(p1)) * vovo(c, j, c, j)
!!!if(ibra==11)!!print*,'5ibraibraibra', ibra, - vovo(c, j, c, j)
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

!print*, 'hh z z9 z 13_mem_noR', hh
    end subroutine sub_eom_cc3_13_mem_noR_z9                                                                                                     
        subroutine sub_eom_cc3_13_mem_noR_z10(b, j, c, k, sigrows, d_small, sigma, vdav, ntrial, nocc0, nvirt0, nocc, n0a,n1a,n0i,n1i, eorb, wr)
        real(F64), dimension(:, :), intent(inout) :: sigrows
        real(F64), dimension(:,:), intent(inout) :: d_small
        real(F64), dimension(:), intent(in) :: sigma
        real(F64), intent(in) :: eorb, wr
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0, nocc
        real(F64) :: tmp
    integer, intent(in) :: n0a,n1a,n0i,n1i                                              
            integer, intent(in) :: ntrial, b, j, c, k
            integer :: p1, p2, ai, bj, ibra 
        integer :: hh
        hh = 1

            
    if (c >= n0a .and. c <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
ibra = (c - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial


tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * vovo(b, k, c, j)
!!!if(ibra==11)!!print*,'ibraibraibra', ibra, 30
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
ibra = (c - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial


tmp = (one/(eorb-wr)) * sigma(p1) * vovo(b, j, c, j)
!!!if(ibra==11)!!print*,'ibraibraibra', ibra, 31
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
ibra = (c - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial


tmp = (one/(eorb-wr)) * sigma(p1) * vovo(b, j, c, k)
!!!if(ibra==11)!!print*,'ibraibraibra', ibra, 32
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (b >= n0a .and. b <= n1a) then 
ibra = (b - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial
hh = hh + 1 

tmp =  (one/(eorb-wr)) * (- sigma(p1)) * vovo(c, j, c, j)
!!!if(ibra==11)!!print*,'ibraibraibra', ibra, 33
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

if (b >= n0a .and. b <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
ibra = (b - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial
hh = hh + 1 

tmp = (one/(eorb-wr)) * sigma(p1) * vovo(c, j, c, k)
!!!if(ibra==11)!!print*,'ibraibraibra', ibra, 34
do p2 = 1, ntrial
        !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!!print*, 'kaktus', vdav(p2, ibra), tmp,	vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) +  vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp

        
         end do 

end if 
end if 

!print*, 'hh z z10 z 13_mem_noR', hh
    end subroutine sub_eom_cc3_13_mem_noR_z10
    end module eom_cc3_13_mem_noR
    
