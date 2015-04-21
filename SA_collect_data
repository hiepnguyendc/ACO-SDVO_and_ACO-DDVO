!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!
! CONTAINS:
! SUBROUTINE SA_collect_run_data(num_point)
!
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

SUBROUTINE SA_collect_run_data(num_sa, num_rand)

	! Aaron Zecchin, July 2002
	! Transfers the ACO solutions stored in store type and transfers them to the point type
	! INPUT: num_sa, num_rand, ant_store[store(1)%cost, store(1)%pen_cost, store(1)%evaluation_num,
	! store(1)%val, store(1)%decision(all)]
	! genesis_network[gnet%max_link]
	! OUTPUT: sensitivity_analysis[sa(num_sa)%rand(num_rand)%cost, sa(num_sa)%rand(num_rand)%pen_cost, 
	! sa(num_sa)%rand(num_rand)%evaluation_num, sa(num_sa)%rand(num_rand)%val, sa(num_sa)%rand(num_rand)%decision(all)]

	USE ant_graph
	USE ant_store
	USE sensitivity_analysis
	USE water_model

	INTEGER :: num_sa, num_rand
	INTEGER :: i,j,l,r

	! storing the best data
	sa(num_sa)%rand(num_rand)%cost           = store(1)%cost
	sa(num_sa)%rand(num_rand)%pen_cost       = store(1)%pen_cost
	sa(num_sa)%rand(num_rand)%val          = store(1)%val
	sa(num_sa)%rand(num_rand)%evaluation_num = store(1)%evaluation_num

	DO i=1,n_tree
		DO j = 1, max_path
            do p = 1, n_sea
                sa(num_sa)%rand(num_rand)%tree(i)%season(p)%dec_crop(j) = store(1)%tree(i)%season(p)%dec_crop(j)
			    do r = 1, n_crop(p)
				    sa(num_sa)%rand(num_rand)%tree(i)%season(p)%crop(r)%dec_water(j) = &
                        store(1)%tree(i)%season(p)%crop(r)%dec_water(j)
				    sa(num_sa)%rand(num_rand)%tree(i)%season(p)%crop(r)%net_return(j) = &
                        store(1)%tree(i)%season(p)%crop(r)%net_return(j)
				    sa(num_sa)%rand(num_rand)%tree(i)%season(p)%crop(r)%water_use(j) = &
                        store(1)%tree(i)%season(p)%crop(r)%water_use(j)
				    sa(num_sa)%rand(num_rand)%tree(i)%season(p)%crop(r)%area_planted(j) = &
                        store(1)%tree(i)%season(p)%crop(r)%area_planted(j)
                end do
            end do
		END DO
	END DO
END SUBROUTINE SA_collect_run_data
