!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!
! CONTAINS:
! SUBROUTINE path_selection_wateruse(num_it, num_ant,itr,dpts)
! SUBROUTINE selection_standard_wateruse(num_ant,itr,dpts)
!
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  
  
 subroutine path_selection_wateruse(num_it,num_ant,itr,dpts,count_dur,cur_sea)
 
! Aaron Zecchin, April 2002
! calls further subroutines (dependant on aco type) to determine the path that ant "num_ant" is to take
 
   use ant_colony
   use ACO_input
 
   integer :: num_it,num_ant		!current number of iterations and ants
   INTEGER :: itr,dpts,count_dur	!current decision tree and decision point
   integer :: cur_sea               !current season
 
   iF(aco_type==5) CALL selection_standard_wateruse(num_ant,itr,dpts,count_dur,cur_sea)
 
 end subroutine path_selection_wateruse
 
 !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 
 subroutine selection_standard_wateruse(num_ant,itr,dpts,count_dur,cur_sea)
 
 ! Aaron Zecchin, April 2002 modified Joanna Szemis, October 2010, modified by Duc Cong Hiep Nguyen, February 2014
 ! determines path that num_ant is to take
 
	use ant_graph
	use ant_colony
	use r_num
    use water_model
   
	real(8) :: sum_prob_water, wuse
	integer :: flag,j,i,r,q, checkb, m
	integer :: num_ant,itr,dpts,count_dur
    integer :: cur_sea, cur_crop              !current season, current crop
	real(8) :: sum_area, sum_used_area, water_rest,water_avai
    real(8) :: ran_num
      
    !  selection of edges
    !*************** SELECTION - CROPS *************************
    cur_crop = ant(num_ant)%tree(itr)%season(cur_sea)%dec_crop(dpts)
    if (seasons(cur_sea)%wuse_crop(cur_crop) == 0.0) then
        if (wbstatus == 0) then 
            flag = 0
	        sum_prob_water = 0.00
	        j = 0
            ant(num_ant)%tree(itr)%season(cur_sea)%crop(cur_crop)%random_water(dpts) = grnd()     ! generating random number

	        do while ((flag == 0).AND.(j < tree(itr)%dec(dpts)%season(cur_sea)%crop(cur_crop)%max_opt_water))
		        j = j + 1
                sum_prob_water = sum_prob_water + tree(itr)%dec(dpts)%season(cur_sea)%crop(cur_crop)%opt_water(j)%prob
		        if (ant(num_ant)%tree(itr)%season(cur_sea)%crop(cur_crop)%random_water(dpts) <= sum_prob_water) then
			        flag = 1
                    ant(num_ant)%tree(itr)%season(cur_sea)%crop(cur_crop)%dec_water(dpts) = j
		        end if
            end do
    
	        ! This occurs only if ant(num_ant)%random(i) = 1.0
	        if(flag == 0) then
                j = tree(itr)%dec(dpts)%season(cur_sea)%crop(cur_crop)%max_opt_water
                do while ((tree(itr)%dec(dpts)%season(cur_sea)%crop(cur_crop)%opt_water(j)%prob == 0).AND.(j > 1))
                    j = j - 1
                end do
                ant(num_ant)%tree(itr)%season(cur_sea)%crop(cur_crop)%dec_water(dpts) = j
            end if
            !update the option of water use for this crop
            seasons(cur_sea)%wuse_crop(cur_crop) = ant(num_ant)%tree(itr)%season(cur_sea)%crop(cur_crop)%dec_water(dpts)
            !update accumulated water for the current ant
            ant(num_ant)%water_accumulated = ant(num_ant)%water_accumulated&
                + tree(itr)%dec(dpts)%season(cur_sea)%crop(cur_crop)%opt_water(&
                ant(num_ant)%tree(itr)%season(cur_sea)%crop(cur_crop)%dec_water(dpts))%property*array_areas(dpts)
        else    !if (wbstatus == 1)
            do i = 1, n_crop(cur_sea-1)
                if (seasons(cur_sea-1)%name_crop(i) == seasons(cur_sea)%name_crop(cur_crop)) then
                    ant(num_ant)%tree(itr)%season(cur_sea)%crop(cur_crop)%dec_water(dpts) = &
                        ant(num_ant)%tree(itr)%season(cur_sea-1)%crop(i)%dec_water(dpts)
                    seasons(cur_sea)%wuse_crop(cur_crop) = &
                        ant(num_ant)%tree(itr)%season(cur_sea)%crop(cur_crop)%dec_water(dpts)
                end if
            end do
        end if
    else    !if (seasons(cur_sea)%wuse_crop(cur_crop) /= 0.0) 
        ant(num_ant)%tree(itr)%season(cur_sea)%crop(cur_crop)%dec_water(dpts) = seasons(cur_sea)%wuse_crop(cur_crop)
        !update accumulated water for the current ant
        if (wbstatus == 0) then 
            ant(num_ant)%water_accumulated = ant(num_ant)%water_accumulated&
                + tree(itr)%dec(dpts)%season(cur_sea)%crop(cur_crop)%opt_water(&
                ant(num_ant)%tree(itr)%season(cur_sea)%crop(cur_crop)%dec_water(dpts))%property*array_areas(dpts)
        end if
    end if

end subroutine selection_standard_wateruse
