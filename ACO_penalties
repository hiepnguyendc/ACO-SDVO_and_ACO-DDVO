!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
subroutine check_penalties(num_ant)

! Duc Cong Hiep Nguyen, February 2014

    use ant_graph
	use ant_colony

	integer :: num_ant,itr,dpts,count_dur,cur_sea
    
    ! check the penalty of maximum and minimum of crops
    call penalty_sea_max_area(num_ant)
    call penalty_crop_maxmin_area(num_ant)
    call penalty_wuse(num_ant)
    
end subroutine check_penalties

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
subroutine penalty_sea_max_area(num_ant)

! Duc Cong Hiep Nguyen, February 2014

    use ant_graph
	use ant_colony
   
	integer :: i
	integer :: num_ant
    
    do i = 1, n_sea
        if (ant(num_ant)%sea_cur_area(i) > sea_max_area(i)) then
            pen_maxsea = ant(num_ant)%sea_cur_area(i) - sea_max_area(i)
        end if
    end do
    
end subroutine penalty_sea_max_area
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

subroutine penalty_crop_maxmin_area(num_ant)

!Duc Cong Hiep Nguyen, February 2014

    !check the crops obtaining the maximum values
    
    use ant_graph
	use ant_colony
   
	integer :: i, j, k
	integer :: num_ant
    
    do i = 1,n_tree
        do j = 1, n_sea
            do k = 1, n_crop(j)
                if (ant(num_ant)%tree(i)%season(j)%crop(k)%area_accumulated > &
                    seasons(j)%max_crop_area(k)) then
                    pen_maxcrop = ant(num_ant)%tree(i)%season(j)%crop(k)%area_accumulated &
                        - seasons(j)%max_crop_area(k)
                    go to 100
                end if
                if (ant(num_ant)%tree(i)%season(j)%crop(k)%area_accumulated < &
                    seasons(j)%min_crop_area(k)) then
                    pen_mincrop = seasons(j)%min_crop_area(k) - &
                        ant(num_ant)%tree(i)%season(j)%crop(k)%area_accumulated
                    go to 100
                end if
            end do
        end do
    end do
    
100 i = i + 1
    
end subroutine penalty_crop_maxmin_area
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

subroutine penalty_wuse(num_ant)

! Duc Cong Hiep Nguyen, February 2014

    use ant_graph
	use ant_colony
    use water_model
   
	integer :: num_ant
    
! check the penalty of water use
    if (ant(num_ant)%water_accumulated > al_yr_res) then     
        pen_wuse = ant(num_ant)%water_accumulated - al_yr_res
    end if
    
end subroutine penalty_wuse
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
