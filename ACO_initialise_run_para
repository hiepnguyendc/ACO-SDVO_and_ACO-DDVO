 !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!
! CONTAINS:
! SUBROUTINE initialise_ACO_runtime_parameters
! SUBROUTINE initialise_runtime_MMAS 
! SUBROUTINE initialise_runtime_MMAS
!
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
subroutine initialise_ACO_runtime_parameters
  
    ! Aaron Zecchin, April 2002,modified by Joanna Szemis, October  2010
    ! This subroutine re-initialises the pheromone intensities on the paths and calls subroutines to
    ! re-initialise the run-dependant variables (i.e. best ranking ant etc.) for a new ACO optimisation run
  
    use ant_graph
    use ACO_input
    
    integer :: i,j,k,l,r,p 
  
    do i=1,n_tree
		do j=1,max_path			! why use n_dpts?
            do p = 1, n_sea
                do r=1,tree(i)%dec(j)%season(p)%max_opt_crop
                    tree(i)%dec(j)%season(p)%opt_crop(r)%tau = tau_0
                end do
            
			    do l=1,n_crop(p)
                    if (seasons(p)%name_crop(l) /= "dryland") then
				        do r=1,tree(i)%dec(j)%season(p)%crop(l)%max_opt_water					
					        tree(i)%dec(j)%season(p)%crop(l)%opt_water(r)%tau = tau_0
                        end do
                    end if
                end do
            end do
		end do      
    end do
  
    IF(aco_type == 5) CALL initialise_runtime_MMAS
    
    call initialise_runtime_ant_store
     
  end subroutine initialise_ACO_runtime_parameters
  
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

SUBROUTINE initialise_runtime_MMAS

    USE para_MMAS
   
    REAL :: high_val

    high_val = 10.0**20.0

! initialising value1 of best solution (iteration best and global best) to arbitrarily high value1

    best_g%val = high_val
    best_i%val = high_val

! initialising global updating counter

    count_g = 0

    END SUBROUTINE initialise_runtime_MMAS

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

subroutine initialise_runtime_ant_store

! Aaron Zecchin, July 2002
! INPUT: ant_store[max_store]
! OUNPUT: ant_store[store(all)%value1]

    use ant_store

    integer :: i            
    real(8) :: high_val

    high_val = 10.0**20.0

    do i = 1, max_store
        store(i)%val = high_val
    end do

  end subroutine initialise_runtime_ant_store
