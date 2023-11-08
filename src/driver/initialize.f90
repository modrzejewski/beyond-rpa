module initialize
      use gparam
      use gto
      use images
      use basis
      use linalg
      use xcfunc
      use fock2el
      use threads
      use fbuild
      use auxint
      use ecpint
      use auto2e
      use display

      implicit none

contains

      subroutine init_modules()
            call gto_init()
            call linalg_init()
            call images_init()
            if (JOBTYPE /= JOB_REAL_UKS_SP .and. &
                  JOBTYPE /= JOB_REAL_UKS_INT .and. &
                  JOBTYPE /= JOB_REAL_UKS_RPA) then
                  call msg("WARNING: Loading deprecated module THREADS", MSG_WARNING)
                  call threads_init()
            end if
            !
            ! FIXME: change if analytic gradient is implemented
            !
            call boys_init(4 * AUTO2E_MAXL)
            call auto2e_init()
            call fock2el_init()
            call xcfunc_init()
            call fbuild_init()
      end subroutine init_modules

      
      subroutine free_modules()
            call gparam_free()
            call gto_free()
            call linalg_free()
            call grid_free()
            call images_free()
            if (JOBTYPE /= JOB_REAL_UKS_SP .and. &
                  JOBTYPE /= JOB_REAL_UKS_INT .and. &
                  JOBTYPE /= JOB_REAL_UKS_RPA) then
                  call threads_free()
            end if
            call boys_free()
            call fock2el_free()
            call xcfunc_free()
            call fbuild_free()
            call aux_free()
            call ecp_free()
      end subroutine free_modules
end module initialize
