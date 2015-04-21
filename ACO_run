!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!
! CONTAINS:
! SUBROUTINE ACO_run
!
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

 subroutine ACO_run
   ! Aaron Zecchin, April 2003, modified by Joanna Szemis October 2010
   ! modified by Duc Cong Hiep Nguyen, February 2014
   ! ACO_run performs all the required routines for a single run of ACO
      
	use ACO_input
	use ant_colony
	use ant_graph
   
	integer :: n_it,n_ant,i,p,q
	integer :: j,k,count,count2
	integer :: l,r
    real(8) :: cuarea
    
	call initialise_ACO_runtime_parameters
   
	do n_it = 1, max_it                    ! entering iteration loop        
 		do n_ant = 1, max_ant                 ! entering ant loop          
            ant(n_ant)%water_accumulated = 0.0
            do i=1,n_tree
                do p = 1, n_sea
                    sea_min_area(p) = sum(seasons(p)%min_crop_area(:))
                                        
                    ant(n_ant)%sea_cur_area(p) = 0
                    ant(n_ant)%sea_cur_dryarea(p) = 0
                    
                    seasons(p)%wuse_crop(:) = 0
                    
                    do k = 1, n_crop(p)                 
                        ant(n_ant)%tree(i)%season(p)%crop(k)%max_area = seasons(p)%max_crop_area(k)
                        ant(n_ant)%tree(i)%season(p)%crop(k)%area_accumulated = 0
                        ant(n_ant)%tree(i)%season(p)%crop(k)%area_planted = 0
                        ant(n_ant)%tree(i)%season(p)%crop(k)%dec_water(:) = 1
                    end do
                end do
                
				!Initialising the counter for the duration which cuts out possible routes
				do q = 1, max_path
                    bstatus = 0
                    wbstatus = 0
                    do p = 1, n_sea
                        ant(n_ant)%tree(i)%season(p)%sea_status = 0
                        ant(n_ant)%tree(i)%season(p)%max_status(:) = 0
                        ant(n_ant)%tree(i)%season(p)%min_status(:) = 0
                        ant(n_ant)%tree(i)%season(p)%crop(:)%water_status = 0

                        call check_constraints(n_it,n_ant,i,q,1,p)	            ! check constraints
                  
					    call prob_determination(n_it,aco_type,i,n_ant,q,1,p)	! sets probabilities for ant loop 
					    call path_selection(n_it,n_ant,i,q,1,p)				    ! calling path selection subroutine                  
                        if (seasons(p)%name_crop(ant(n_ant)%tree(i)%season(p)%dec_crop(q)) /= "dryland") then
                            ant(n_ant)%tree(i)%season(p)%crop(ant(n_ant)%tree(i)%season(p)%dec_crop(q))%dec_water_status(:) = 0
                            call constraints_avai_crop_water (n_it,n_ant,i,q,1,p)
                            call prob_determination_wateruse(n_it,aco_type,i,n_ant,q,1,p)
                            call path_selection_wateruse(n_it,n_ant,i,q,1,p)                       
                        end if
                    end do
                end do
			end do
			call path_evaluation(n_it, n_ant)
		end do
		call global_routines(n_it)  
	end do
  
211 FORMAT(2x, I3, 2x, I3, 2x, I3, 2x, I3)
 end subroutine
