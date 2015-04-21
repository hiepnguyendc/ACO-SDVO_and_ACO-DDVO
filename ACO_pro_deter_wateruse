!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!
! CONTAINS:
! SUBROUTINE prob_determination_wateruse(num_it, aco_type,itr,n_ant,dpts)
! SUBROUTINE prob_AS_wateruse(itr,n_ant,dpts)
! SUBROUTINE prob_MMAS_wateruse(itr,n_ant,dpts)
! SUBROUTINE prob_standard_wateruse(alpha, beta,itr,n_ant,dpts)
!
!::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: 
   
  subroutine prob_determination_wateruse(num_it, aco_type,itr,n_ant,dpts,count_dur,cur_sea)

! Aaron Zecchin, April 2002, modified by Joanna Szemis
! Determines probability distrubutions for each decision point for the selected aco if the ACO type is not ACS
! calls print_prob

    INTEGER :: num_it,aco_type				!number of current iterations, aco type chosen
    INTEGER :: n_ant,dpts,itr,count_dur		!number of current ants, decision points and tree
    INTEGER :: dum = 1						!Passed into print_prob as this variable location is only used for ACS
    integer :: cur_sea                      !current season

    IF(aco_type==5) CALL prob_MMAS_wateruse(itr,n_ant,dpts,count_dur,cur_sea)

   end subroutine prob_determination_wateruse
    
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    
  subroutine prob_AS_wateruse(itr,n_ant,dpts,count_dur,cur_sea)

! Aaron Zecchin, April 2002
! Call prob_standard with AS parameters

    use para_as
    integer::n_ant,dpts,itr,count_dur		!number of current ants, decision points and tree
    integer :: cur_sea               !current season

    CALL prob_standard_wateruse(alpha,beta,itr,n_ant,dpts,count_dur,cur_sea)

  end subroutine prob_AS_wateruse
     
  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    SUBROUTINE prob_MMAS_wateruse(itr,n_ant,dpts,count_dur,cur_sea)

! Aaron Zecchin, April 2002
! Call prob_standard with MMAS parameters

    USE para_mmas

    integer::n_ant,dpts,itr,count_dur		!number of current ants, decision points and tree
    integer :: cur_sea               !current season
    
    CALL prob_standard_wateruse(alpha,beta,itr,n_ant,dpts,count_dur,cur_sea)

    END SUBROUTINE prob_MMAS_wateruse

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::   
 
subroutine prob_standard_wateruse(alpha,beta,itr,n_ant,dpts,count_dur,cur_sea)

! Aaron Zecchin, April 2002, modified by Joanna Szemis, October 2010, modified by Duc Cong Hiep Nguyen, February 2014
! Determines the probability distribution using generic ACO weighting function
! INPUT: alpha, beta, ant-graph[ max_path, path(i)%max_edge, path(i)%edge(j)%eta, path(i)%edge(j)%tau ]
! OUTPUT: ant_graph[ path(i)%edge(j)%prob ]
! Note i = 1, max_path, and j = 1, path(i)%max_edge

    use ant_graph
    use ant_colony
    use water_model
	
	real(8) :: alpha, beta				
    real(8) :: tot_prob_crop
	real(8) :: tot_prob_water
    real(8) :: tot_prob_land
    integer :: n_ant,dpts,itr,count_dur
    integer :: k,l,r,m
    integer :: cur_sea, cur_crop              !current season, current crop
    real(8) :: y, tvar

!first two loops to calculate the decision points to choose the water amounts for crops
    cur_crop = ant(n_ant)%tree(itr)%season(cur_sea)%dec_crop(dpts)
    if (seasons(cur_sea)%wuse_crop(cur_crop) == 0.0) then
        if (wbstatus == 0) then
            tot_prob_water = 0.00				
		    do r = 1,tree(itr)%dec(dpts)%season(cur_sea)%crop(cur_crop)%max_opt_water
                if (ant(n_ant)%tree(itr)%season(cur_sea)%crop(cur_crop)%dec_water_status(r) == 0) then
                    y = tree(itr)%dec(dpts)%season(cur_sea)%crop(cur_crop)%opt_water(r)%property
                        tvar = (seasons(cur_sea)%aa(cur_crop,1)*seasons(cur_sea)%aa(cur_crop,2) + &
                            seasons(cur_sea)%bb(cur_crop,1)*seasons(cur_sea)%bb(cur_crop,2)*y + &
                            seasons(cur_sea)%cc(cur_crop,1)*seasons(cur_sea)%cc(cur_crop,2)&
                            *(y**seasons(cur_sea)%cc(cur_crop,3)))*seasons(cur_sea)%pcrop(cur_crop)-&
                            (seasons(cur_sea)%pcost(cur_crop)+pwater*y)
                        if (tvar > 0) then
			                tree(itr)%dec(dpts)%season(cur_sea)%crop(cur_crop)%opt_water(r)%prob=&
                                tree(itr)%dec(dpts)%season(cur_sea)%crop(cur_crop)%opt_water(r)%tau**alpha &
				                *tree(itr)%dec(dpts)%season(cur_sea)%crop(cur_crop)%opt_water(r)%heu**beta
			                tot_prob_water=tot_prob_water+tree(itr)%dec(dpts)%season(cur_sea)%crop(cur_crop)%opt_water(r)%prob
                        else
                            tree(itr)%dec(dpts)%season(cur_sea)%crop(cur_crop)%opt_water(r)%prob = 0.0
                        end if
                else  !if (ant(n_ant)%tree(itr)%season(cur_sea)%crop(cur_crop)%dec_water_status(r) /= 0)
                    tree(itr)%dec(dpts)%season(cur_sea)%crop(cur_crop)%opt_water(r)%prob = 0.0
                end if !if (ant(n_ant)%tree(itr)%season(cur_sea)%crop(cur_crop)%dec_water_status(r) == 0)
		    end do

		    ! Actual prob calculations
            if (tot_prob_water > 0) then
		        do r = 1,tree(itr)%dec(dpts)%season(cur_sea)%crop(cur_crop)%max_opt_water
			        tree(itr)%dec(dpts)%season(cur_sea)%crop(cur_crop)%opt_water(r)%prob=&
                        tree(itr)%dec(dpts)%season(cur_sea)%crop(cur_crop)%opt_water(r)%prob/tot_prob_water
                end do
            end if
        else    !if (wbstatus == 1)
            tree(itr)%dec(dpts)%season(cur_sea)%crop(cur_crop)%opt_water(:)%prob = 0.0
        end if
    end if !seasons(cur_sea)%wuse_crop(cur_crop) == 0.0

211 FORMAT(2x, I3, 2x, I3, 2x, I3, 2x, I3)

end subroutine prob_standard_wateruse
 
    
