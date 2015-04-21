
subroutine global_routines(num_it)

  use ACO_input
  
  integer :: num_it

    IF(aco_type==5) CALL global_updating_MMAS
    
    CALL global_store_results(num_it)

end subroutine global_routines

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 
SUBROUTINE global_updating_MMAS

	! Aaron Zecchin, April 2002, modified by Duc Cong Hiep Nguyen, February 2014
	! Updates pheromone levels according to ASrank
	! INPUT: ant_colony[ant(l)%decision(j), ant(l)%value1, max_ant], ant_graph[path(i)%edge(j)%tau],
	! para_asrank[best_i&decision(j), best_i%value1,best_g&decision(j), best_g%value1, roe, pbest, delta, counter_g]
	! OUTPUT: ant_graph[path(i)%edge(j)%tau], para_mmas[best%decision(j), best%value1, count_g]
	! Note: l = 1, max_ant, i = 1, max_path, and j = 1, path(i)%max_edge

	USE ant_colony
	USE ant_graph
	USE para_mmas

	INTEGER :: i, j, k, l, r                        ! counters
	INTEGER :: sum_path_water, sum_path_crop  	    ! used to determine tau_min
	integer :: sel_water,sel_crop                   ! used to determine tau_min
	REAL(8) :: tau_current_water, tau_new_water     ! pheromone value1s
    REAL(8) :: tau_current_crop, tau_new_crop       ! pheromone value1s
	REAL(8) :: avg_water, avg_crop                  ! used to determine tau_min
	REAL(8) :: small_val, high_val

	small_val = 0.0000000001
	high_val = 10.0**20.0

	! Decaying current pheromone levels
	do i=1,n_tree
		do j = 1, max_path
            do p = 1, n_sea
                do r = 1, tree(i)%dec(j)%season(p)%max_opt_crop
                    tree(i)%dec(j)%season(p)%opt_crop(r)%tau = tree(i)%dec(j)%season(p)%opt_crop(r)%tau * rho
                end do
			    do l = 1, n_crop(p)		
				    do r = 1, tree(i)%dec(j)%season(p)%crop(l)%max_opt_water
					    tree(i)%dec(j)%season(p)%crop(l)%opt_water(r)%tau = &
                            tree(i)%dec(j)%season(p)%crop(l)%opt_water(r)%tau * rho 
                    end do
                end do
            end do
		end do
	end do   

! determining global best path/solution
	DO i = 1, max_ant     
		IF (ant(i)%val < best_g%val) THEN
			best_g%val = ant(i)%val
			best_g%pen_cost = ant(i)%pen_cost
			best_g%cost = ant(i)%cost
			DO k = 1, n_tree
				DO j = 1, max_path
                    do p = 1, n_sea
                        best_g%tree(k)%season(p)%dec_crop(j) = ant(i)%tree(k)%season(p)%dec_crop(j)
					    do r = 1, n_crop(p)
						    best_g%tree(k)%season(p)%crop(r)%dec_water(j) = ant(i)%tree(k)%season(p)%crop(r)%dec_water(j)
                        end do
                    end do
				END DO
			END DO
		END IF
	END DO

	! determining iteration best path/solution
	DO i = 1, max_ant
		IF (ant(i)%val < best_i%val) THEN
			best_i%val = ant(i)%val
			best_i%pen_cost = ant(i)%pen_cost
			best_i%cost = ant(i)%cost
			DO k = 1, n_tree
				DO j = 1, max_path
                    do p = 1, n_sea
                        best_i%tree(k)%season(p)%dec_crop(j) = ant(i)%tree(k)%season(p)%dec_crop(j)
					    do r = 1, n_crop(p)
						    best_i%tree(k)%season(p)%crop(r)%dec_water(j) = ant(i)%tree(k)%season(p)%crop(r)%dec_water(j)
                        end do
                    end do
				END DO
			END DO
		END IF
	END DO

	! updating best (global or iteration) ant paths
	count_g = count_g + 1
	IF(count_g == freq_g) THEN
		! global best updating
		IF(best_g%val < small_val)THEN              ! determining amount of pheromone to add
			tau_add = q / small_val
		ELSE
			tau_add = q / best_g%val
		END IF
     
		DO k = 1, n_tree
			DO i = 1, max_path
                do p = 1, n_sea
                    sel_crop = best_g%tree(k)%season(p)%dec_crop(i)
				    tau_current_crop = tree(k)%dec(i)%season(p)%opt_crop(sel_crop)%tau
				    tau_new_crop = tau_current_crop + tau_add
				    tree(k)%dec(i)%season(p)%opt_crop(sel_crop)%tau = tau_new_crop
				    do l = 1, n_crop(p)
					    sel_water = best_g%tree(k)%season(p)%crop(l)%dec_water(i)
					    tau_current_water = tree(k)%dec(i)%season(p)%crop(l)%opt_water(sel_water)%tau
					    tau_new_water = tau_current_water + tau_add
					    tree(k)%dec(i)%season(p)%crop(l)%opt_water(sel_water)%tau = tau_new_water
                    end do
                end do
			END DO
		END DO
		count_g = 0
	END IF    

	! iteration best updating
	IF (best_i%val < small_val) THEN              ! determining amount of pheromone to add
		tau_add = q / small_val
	ELSE
		tau_add = q / best_i%val
	END IF

	DO k = 1, n_tree
		DO i = 1, max_path
            do p = 1, n_sea
                sel_crop = best_i%tree(k)%season(p)%dec_crop(i)
			    tau_current_crop = tree(k)%dec(i)%season(p)%opt_crop(sel_crop)%tau
			    tau_new_crop = tau_current_crop + tau_add
			    tree(k)%dec(i)%season(p)%opt_crop(sel_crop)%tau = tau_new_crop
			    do l = 1, n_crop(p)
				    sel_water = best_i%tree(k)%season(p)%crop(l)%dec_water(i)
				    tau_current_water = tree(k)%dec(i)%season(p)%crop(l)%opt_water(sel_water)%tau
				    tau_new_water = tau_current_water + tau_add
				    tree(k)%dec(i)%season(p)%crop(l)%opt_water(sel_water)%tau = tau_new_water
                end do
            end do
		END DO
	END DO

	! determining MMAS bounds
	! calculating "tau_max"
	IF (best_g%val < small_val) THEN
		tau_max = 1.0 / (1.0 - rho) * q / small_val
	ELSE
		tau_max = 1.0 / (1.0 - rho) * q / best_g%val
	END IF

	! determining "avg' for use in "tau_min" calc
	sum_path_crop = 0
    sum_path_water = 0
	DO j = 1, n_tree
		DO i = 1, max_path
            do p = 1, n_sea
                sum_path_crop = sum_path_crop + tree(j)%dec(i)%season(p)%max_opt_crop
			    do r = 1, n_crop(p)
				    sum_path_water = sum_path_water + tree(j)%dec(i)%season(p)%crop(r)%max_opt_water
                end do
            end do
		END DO
	END DO

	!**********************times it by num_a because the max path is just one path of decision tree  
	avg_crop = REAL(sum_path_crop) / REAL(max_path)
    avg_water = REAL(sum_path_water) / REAL(max_path)
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!CAREFULL
    
	! calculating "tau_min"
	tau_min_crop = tau_max * (1.0 - pbest**(1.0 / REAL(max_path))) / ((avg_crop - 1.0) * pbest**(1.0 / REAL(max_path)))
    tau_min_water = tau_max * (1.0 - pbest**(1.0 / REAL(max_path))) / ((avg_water - 1.0) * pbest**(1.0 / REAL(max_path)))

	! applying MMAS bounds
	DO k = 1, n_tree
		DO i = 1, max_path
            do p = 1, n_sea
                DO r = 1, tree(k)%dec(i)%season(p)%max_opt_crop
                    IF (tree(k)%dec(i)%season(p)%opt_crop(r)%tau > tau_max) THEN
				        tree(k)%dec(i)%season(p)%opt_crop(r)%tau = tau_max
			        ELSE IF (tree(k)%dec(i)%season(p)%opt_crop(r)%tau < tau_min_crop) THEN
					        tree(k)%dec(i)%season(p)%opt_crop(r)%tau = tau_min_crop
                    END IF
                END DO
			    do l = 1, n_crop(p)
 				    DO r = 1, tree(k)%dec(i)%season(p)%crop(l)%max_opt_water
					    IF (tree(k)%dec(i)%season(p)%crop(l)%opt_water(r)%tau > tau_max) THEN
						    tree(k)%dec(i)%season(p)%crop(l)%opt_water(r)%tau = tau_max
					    ELSE IF (tree(k)%dec(i)%season(p)%crop(l)%opt_water(r)%tau < tau_min_water) THEN
							    tree(k)%dec(i)%season(p)%crop(l)%opt_water(r)%tau = tau_min_water
					    END IF
                    END DO
                end do
            end do
		END DO
	END DO

	! Applying PTS mechanism
	DO k = 1, n_tree
		DO i = 1, max_path
            do p = 1, n_sea
                DO r = 1, tree(k)%dec(i)%season(p)%max_opt_crop
				    tau_current_crop = tree(k)%dec(i)%season(p)%opt_crop(r)%tau
				    tau_new_crop = tau_current_crop + delta * (tau_max - tau_current_crop)
				    tree(k)%dec(i)%season(p)%opt_crop(r)%tau = tau_new_crop
                END DO
			    do l = 1, n_crop(p)
 				    DO r = 1, tree(k)%dec(i)%season(p)%crop(l)%max_opt_water
					    tau_current_water = tree(k)%dec(i)%season(p)%crop(l)%opt_water(r)%tau
					    tau_new_water = tau_current_water + delta * (tau_max - tau_current_water)
					    tree(k)%dec(i)%season(p)%crop(l)%opt_water(r)%tau = tau_new_water
                    END DO	
                end do
            end do
		END DO
    END DO    

END SUBROUTINE global_updating_MMAS
 
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

SUBROUTINE global_store_results(num_it)

	! Aaron Zecchin, April 2002, modified by Duc Cong Hiep Nguyen, February 2014
	! This subroutines updates and stores the global best results
	! INPUT: num_it, ant_graph[max_path, path(all)%edge(selected ants)%property], 
	!      ant_colony[max_ant, ant(all)%val, ant(all)%decision(all)]    
	! OUTPUT: store[all]

	USE ant_colony
	USE ant_graph
	USE ant_store
	USE water_model
    USE sensitivity_analysis

	INTEGER :: num_it,q, r
	INTEGER :: i, j, selection,p,x,position
	INTEGER :: flag      ! used to determine is ant solution is to be stored
	INTEGER :: indicator, count_in!1,indicator2,indicator3 ! indicates how many path selections (for different ants) are the same
	REAL :: diff         ! dummy used to determine equivalence of ant vals
	REAL ::  small_val, high_val, it_best, prop
 
	small_val = 0.00000000001
	high_val = 10.0**20.0

	DO i = 1, max_ant                                       ! start looping through all ants
		! determing if ant lies in top vals to be stored
		! Firstly checking that ant solution is not repeated
		flag = 0
		DO j = 1, max_store
			diff = ABS(ant(i)%val - store(j)%val)
			IF(diff < 0.0001) THEN                                ! Checking for equivalent val
				indicator = 0                                               
				DO x=1,n_tree
					DO k = 1, max_path
                        do p = 1, n_sea
						    do r = 1, n_crop(p)
                                if (ant(i)%tree(x)%season(p)%dec_crop(k) == store(j)%tree(x)%season(p)%dec_crop(k)) then
						            IF (ant(i)%tree(x)%season(p)%crop(r)%dec_water(k) == &
                                            store(j)%tree(x)%season(p)%crop(r)%dec_water(k)) THEN
                                        indicator=indicator+1
                                    END IF
                                end if
                            end do
                        end do
					END DO
				END DO
				count_in = 0
				DO x=1,n_tree
					DO k = 1, max_path
                        do p = 1, n_sea
						    do r = 1, n_crop(p)
							    count_in=count_in+1										
                            end do
                        end do
					END DO
				END DO
				if (indicator == count_in) then 
					flag = 1		 ! If all decisions are equal (i.e. indicator = max_path) then flag = 1
				end if
			END IF                                                
		END DO
      
		!If ant solution is not repeated (i.e. flag = 0) and solution is better than the worst solution stored
		!(i.e.(ant(i)%val < store(max_store)%val)) then it enters into the if statement below
		IF((ant(i)%val < store(max_store)%val).AND.(flag == 0)) THEN
			position = max_store                                          
			!determining position of ant in store array
			DO WHILE ((flag == 0).AND.(position > 1))
				diff = store(position - 1)%val - ant(i)%val
				IF(diff > 0.0) THEN
					position = position - 1
				ELSE
					flag = 1
				END IF
			END DO

			! ordering new ranking, all vals that are below "position" (the location in the ranking of the new ant) 
			! are given vals that are one position higher than they currently are in and the last rank is lost.

			DO j = max_store, position + 1, -1  
				locate = j
				store(locate)%val            = store(locate - 1)%val
				store(locate)%cost           = store(locate - 1)%cost
				store(locate)%pen_cost       = store(locate - 1)%pen_cost
				store(locate)%pen            = store(locate - 1)%pen
				store(locate)%evaluation_num = store(locate - 1)%evaluation_num
				DO q = 1,n_tree
					DO k = 1, max_path
                        do p = 1, n_sea
                            store(locate)%tree(q)%season(p)%dec_crop(k) = store(locate - 1)%tree(q)%season(p)%dec_crop(k)
							do r = 1, n_crop(p)
								store(locate)%tree(q)%season(p)%crop(r)%dec_water(k) = &
                                    store(locate - 1)%tree(q)%season(p)%crop(r)%dec_water(k)
								store(locate)%tree(q)%season(p)%crop(r)%net_return(k) = &
                                    store(locate - 1)%tree(q)%season(p)%crop(r)%net_return(k)
                                store(locate)%tree(q)%season(p)%crop(r)%water_use(k) = &
                                    store(locate - 1)%tree(q)%season(p)%crop(r)%water_use(k)
                                store(locate)%tree(q)%season(p)%crop(r)%area_planted(k) = &
                                    store(locate - 1)%tree(q)%season(p)%crop(r)%area_planted(k)
                            end do
                        end do
					END DO
				END DO
			END DO

			! loading new ranked ant into "position"
			store(position)%val            = ant(i)%val
			store(position)%cost           = ant(i)%cost
			store(position)%pen_cost       = ant(i)%pen_cost
			store(position)%pen            = ant(i)%pen
			store(position)%evaluation_num = i + max_ant * (num_it - 1)
			DO q=1,n_tree
				DO j = 1, max_path
                    do p = 1, n_sea
                        store(position)%tree(q)%season(p)%dec_crop(j) = ant(i)%tree(q)%season(p)%dec_crop(j)
					    do r = 1, n_crop(p)
                            if (r == ant(i)%tree(q)%season(p)%dec_crop(j)) then
						        store(position)%tree(q)%season(p)%crop(r)%dec_water(j) = &
                                    ant(i)%tree(q)%season(p)%crop(r)%dec_water(j)
						        store(position)%tree(q)%season(p)%crop(r)%net_return(j) = &
                                    ant(i)%tree(q)%season(p)%crop(r)%net_return(j)
                                store(position)%tree(q)%season(p)%crop(r)%water_use(j) = &
                                    ant(i)%tree(q)%season(p)%crop(r)%water_use(j)
                                store(position)%tree(q)%season(p)%crop(r)%area_planted(j) = &
                                    ant(i)%tree(q)%season(p)%crop(r)%area_planted(j)
                            end if
                        end do
                    end do
				END DO
			END DO
		END IF                                      ! end if statement searching for ranking ants
    END DO                                       ! end looping through all ants
    
    211 FORMAT(2x, I3, 2x, I3, 1x, I20, 1x, f20.5, 1x, f20.5, 1x, f20.5)

END SUBROUTINE global_store_results

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
