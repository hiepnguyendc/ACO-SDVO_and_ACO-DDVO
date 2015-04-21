subroutine evaluate_objective(val, pure_cost, penalty_cost, penalty,num_ant)
    
! Duc Cong Hiep Nguyen, February 2014

	use ant_graph
	USE water_model
	use ant_colony

	real(8) :: total_pen
    real(8), allocatable, dimension(:) :: sup_irr
	real(8) :: val, pure_cost, penalty_cost, penalty
	REAL(8) :: x,y
	integer :: i,j,k,m,num_ant ,p
    real(8) :: asum
      
	pure_cost = 0.0
	penalty = total_pen
	penalty_cost = penalty*1000.0
		
    do i = 1, n_tree
        do j = 1, max_path
            do p = 1, n_sea
			    do k = 1, n_crop(p)
				    ant(num_ant)%tree(i)%season(p)%crop(k)%net_return(j) = 0.0
                end do
            end do
        end do
    end do
        
    do i = 1, n_tree
        do m = 1, max_path
            do p = 1, n_sea
                k = ant(num_ant)%tree(i)%season(p)%dec_crop(m)
                if (k == 1) then
                    x = 0.0
                else
                    x = tree(i)%dec(m)%season(p)%crop(k)%opt_water(&
                        ant(num_ant)%tree(i)%season(p)%crop(k)%dec_water(m))%property
                end if
                
                ! calculate the amount of water and area used for crop k
                ant(num_ant)%tree(i)%season(p)%crop(k)%water_use(m) = x
                
			    if (x == 0.0) then
				    ant(num_ant)%tree(i)%season(p)%crop(k)%net_return(m) = 0.0
			    else
				    !(edit 08 May 2014)
                    if (seasons(p)%name_crop(k) == "dryland") then    ! dryland
                        ant(num_ant)%tree(i)%season(p)%crop(k)%net_return(m) = 0.0
                    else                ! other crops
                        if ((seasons(p)%aa(k,1)*seasons(p)%aa(k,2) + seasons(p)%bb(k,1)*&
                                seasons(p)%bb(k,2)*x + seasons(p)%cc(k,1)*seasons(p)%cc(k,2)&
                                *(x**seasons(p)%cc(k,3)))*seasons(p)%pcrop(k)&
                                -(seasons(p)%pcost(k)+pwater*x)<0) then
                            ant(num_ant)%tree(i)%season(p)%crop(k)%net_return(m) = 0.0
                        else
                            ant(num_ant)%tree(i)%season(p)%crop(k)%net_return(m) = &
                                (seasons(p)%aa(k,1)*seasons(p)%aa(k,2) + seasons(p)%bb(k,1)*&
                                seasons(p)%bb(k,2)*x + seasons(p)%cc(k,1)*seasons(p)%cc(k,2)&
                                *(x**seasons(p)%cc(k,3)))*seasons(p)%pcrop(k)*array_areas(m)&
                                - array_areas(m)*(seasons(p)%pcost(k)+pwater*x)
                        end if
                    end if
                end if
            end do
        end do
    end do
        
    do i = 1, n_tree
        do j = 1, max_path
            do p = 1, n_sea
			    do k = 1, n_crop(p)
                    if (seasons(p)%bsea(k) /= 1) then
				        pure_cost = pure_cost + ant(num_ant)%tree(i)%season(p)%crop(k)%net_return(j)
                    end if
                end do
            end do
        end do
    end do
        
	!objective function
    if (pure_cost == 0.0) then
		val = 1000000.0
	else
		val = 1000000.0/(pure_cost+1000000.0)
	end if

end subroutine evaluate_objective
 
 
 
