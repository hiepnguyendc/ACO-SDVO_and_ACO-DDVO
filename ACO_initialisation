!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!
! CONTAINS:
! SUBROUTINE initialise_ACO
! SUBROUTINE initialise_ant_graph
! SUBROUTINE initialise_AS
! SUBROUTINE initialise_MMAS
!
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
subroutine ACO_initialisation
	! Aaron Zecchin, April 2002, modified by Joanna Szemis, Ocotber 2010, modified by Duc Cong Hiep Nguyen, October 2012
	! this subroutine initialises all aco parameters
    
	use ACO_input
	use ant_colony
	use ant_store
	use ant_graph
	USE ant_print_control
	USE para_as
	USE r_num

	integer :: i,j,k,l

	open(unit=22, file="ACO_input.txt", status="old")

	read(22,*) max_it   
	read(22,*) max_ant
	read(22,*) max_store
	read(22,*) zero_cost
	read(22,*) seed_ran_1, seed_ran_2, seed_ran_3, seed_ran_4, seed_ran_5
	read(22,*) pbest
	read(22,*) tau_0
	read(22,*) aco_type
    read(22,*) print_list,print_num,print_sum
    
	call initialise_ant_graph
	 
	allocate(ant(max_ant))
	do i = 1, max_ant
        allocate(ant(i)%tree(n_tree))
		do l = 1, n_tree
            allocate(ant(i)%tree(l)%season(n_sea))
            do m = 1, n_sea
                allocate(ant(i)%tree(l)%season(m)%crop(n_crop(m)))
                allocate(ant(i)%tree(l)%season(m)%max_status(n_crop(m)))
                ant(i)%tree(l)%season(m)%max_status(:) = 0
                allocate(ant(i)%tree(l)%season(m)%min_status(n_crop(m)))
                ant(i)%tree(l)%season(m)%min_status(:) = 0
                allocate(ant(i)%tree(l)%season(m)%dec_crop(max_path))
                allocate(ant(i)%tree(l)%season(m)%random_crop(max_path))
                allocate(ant(i)%tree(l)%season(m)%check_crop(n_crop(m)))
                do k = 1, n_crop(m)
			        allocate(ant(i)%tree(l)%season(m)%crop(k)%dec_water(max_path))
                    ant(i)%tree(l)%season(m)%crop(k)%dec_water(:) = 1
			        allocate(ant(i)%tree(l)%season(m)%crop(k)%random_water(max_path))
			        allocate(ant(i)%tree(l)%season(m)%crop(k)%net_return(max_path))
                    ant(i)%tree(l)%season(m)%crop(k)%net_return(:) = 0.0
   			        allocate(ant(i)%tree(l)%season(m)%crop(k)%water_use(max_path))
                    ant(i)%tree(l)%season(m)%crop(k)%water_use(:) = 0.0
                    allocate(ant(i)%tree(l)%season(m)%crop(k)%area_planted(max_path))
                    ant(i)%tree(l)%season(m)%crop(k)%area_planted(:) = 0.0
                    ant(i)%tree(l)%season(m)%crop(k)%water_status = 0
                    allocate(ant(i)%tree(l)%season(m)%crop(k)%dec_water_status(seasons(m)%n_opt_water(k)))
                end do
            end do
		end do
    end do
    
	! setting array sizes for storage array
	allocate(store(max_store))
	do i = 1, max_store
        allocate(store(i)%tree(n_tree))
		do l = 1, n_tree
            allocate(store(i)%tree(l)%season(n_sea))
            do j = 1, n_sea
		        allocate(store(i)%tree(l)%season(j)%crop(n_crop(j)))
                allocate(store(i)%tree(l)%season(j)%dec_crop(max_path))
                do k = 1, n_crop(j)            
			        allocate(store(i)%tree(l)%season(j)%crop(k)%dec_water(max_path))
			        allocate(store(i)%tree(l)%season(j)%crop(k)%net_return(max_path))
                    allocate(store(i)%tree(l)%season(j)%crop(k)%water_use(max_path))
                    allocate(store(i)%tree(l)%season(j)%crop(k)%area_planted(max_path))
                end do
            end do
        end do
    end do

	! Initialising appropriate ACO type parameters (reading in second line)
	if(aco_type == 5) call initialise_MMAS   

end subroutine ACO_initialisation

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
! Joanna Szemis, Ocotber 2010, modified by Duc Cong Hiep Nguyen, October 2012
subroutine initialise_ant_graph

	use ant_graph
	use ACO_input
	USE water_model
   
	integer :: i,j,k,l,r,h
	real(8) :: small_val = 0.00000001
    real(8), allocatable, dimension(:) :: maxdummy, mindummy, pcropdummy,pcostdummy
   
	open(unit=33, file="input_decision_tree.txt", status="unknown")
   
	!reading in values from a file 
	read(33,*) n_tree
	allocate(tree(n_tree))
    
	do i=1,n_tree
        read(33,*)      !tree i
        read(33,*) n_dpts       !number of sub-areas
        allocate(array_areas(n_dpts))
        read(33,*) unit_area
        array_areas(1:n_dpts) = [5,6,7,5,6,7,5,6,7,5,6,7,5,6,7,5,6,7,5,6,7,5,6,7,5,6,7,5,6]
        maxA_region = sum(array_areas(1:n_dpts))
        read(33,*) n_sea   !number of sub-season
        read(33,*) n_crop(1), n_crop(2), n_both
        max_path = n_dpts
        allocate(tree(i)%dec(max_path))
        allocate(seasons(n_sea))
        do j = 1, n_sea
            allocate(seasons(j)%name_crop(n_crop(j)))
            allocate(seasons(j)%wuse_crop(n_crop(j)))
            allocate(seasons(j)%n_opt_water(n_crop(j)))
            allocate(seasons(j)%ws_sea(n_crop(j)))
            allocate(seasons(j)%bsea(n_crop(j)))
            allocate(seasons(j)%aa(n_crop(j),3))
            allocate(seasons(j)%bb(n_crop(j),3))
            allocate(seasons(j)%cc(n_crop(j),3))
            allocate(seasons(j)%max_crop_area(n_crop(j)))
            allocate(seasons(j)%min_crop_area(n_crop(j)))
            allocate(seasons(j)%pcrop(n_crop(j)))
            allocate(seasons(j)%pcost(n_crop(j)))
        end do
        do k=1,max_path
            allocate(tree(i)%dec(k)%season(n_sea))
            do j = 1, n_sea
			    allocate(tree(i)%dec(k)%season(j)%crop(n_crop(j)))
                allocate(tree(i)%dec(k)%season(j)%opt_crop(n_crop(j)))
            end do
        end do
    end do
    
    do i=1,n_tree
        do j = 1, n_sea
            read(33,*)
            read(33,*) sea_max_area(j)  ! read maximum area for each season
            read(33,*) (seasons(j)%name_crop(k),k=1,n_crop(j),1)    ! read name of all crops

            ! read information to know whether a crop is planted in winter or summer season
            read(33,*) (seasons(j)%ws_sea(k),k=1,n_crop(j),1)    
            allocate(maxdummy(n_crop(j)))
            read(33,*) (maxdummy(k),k=1,n_crop(j),1)
            allocate(mindummy(n_crop(j)))
            read(33,*) (mindummy(k),k=1,n_crop(j),1)
            allocate(pcropdummy(n_crop(j)))
            read(33,*) (pcropdummy(k),k=1,n_crop(j),1)
            allocate(pcostdummy(n_crop(j)))
            read(33,*) (pcostdummy(k),k=1,n_crop(j),1)
            do l=1,n_crop(j)
	            seasons(j)%max_crop_area(l) = maxdummy(l)
	            seasons(j)%min_crop_area(l) = mindummy(l)
	            seasons(j)%pcrop(l) = pcropdummy(l)
                seasons(j)%pcost(l) = pcostdummy(l)
            end do
            read(33,*)
            deallocate(maxdummy)
            deallocate(mindummy)
            deallocate(pcropdummy)
            deallocate(pcostdummy)
            seasons(j)%bsea(:) = 0
        end do
    end do
    
    do l=1,n_crop(2)
        if (seasons(2)%ws_sea(l) == 3) then
            seasons(2)%bsea(l) = 1
        end if
    end do
    
    read (33,*) wcap
    
	call cost_details
        
	!for each decision path avaible for options 1 and 2 assiging the heuritic factor = 0 and 1 respectively
	  
	do i=1,n_tree
		do j=1,max_path
            do k = 1, n_sea
                tree(i)%dec(j)%season(k)%max_opt_crop = n_crop(k)
			    do r=1,n_crop(k)			
				    if (tree(i)%dec(j)%season(k)%opt_crop(r)%cost < small_val) then	!Checking to see if cost is equal to zero
					    tree(i)%dec(j)%season(k)%opt_crop(r)%heu = 1.0 / zero_cost		!if so then a virtual cost is used to determine visability
				    else																	!if not the actual option cost is used
					    tree(i)%dec(j)%season(k)%opt_crop(r)%heu = (1.0 - 1.0 /tree(i)%dec(j)%season(k)%opt_crop(r)%cost)
                    end if
                end do
            
                tree(i)%dec(j)%season(k)%crop(1)%max_opt_water = 1
			    do l=2,n_crop(k),1
                    tree(i)%dec(j)%season(k)%crop(l)%max_opt_water = seasons(k)%n_opt_water(l)
				    do r=1,seasons(k)%n_opt_water(l)
					    if (tree(i)%dec(j)%season(k)%crop(l)%opt_water(r)%cost < small_val) then	!Checking to see if cost is equal to zero
						    tree(i)%dec(j)%season(k)%crop(l)%opt_water(r)%heu = 1.0         		!if so then a virtual cost is used to determine visability
					    else																    	!if not the actual option cost is used
						    tree(i)%dec(j)%season(k)%crop(l)%opt_water(r)%heu = 1.0 	
					    end if
                    end do
                end do
            end do
		end do  
	end do
   
	close(33)

end subroutine initialise_ant_graph

!::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
subroutine initialise_AS
    
	use para_as

	open(unit=44, file="para_type.txt", status="unknown")
	read(44,*) alpha
	read(44,*) beta
	read(44,*) rho
	read(44,*) q
   
end subroutine initialise_AS
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
SUBROUTINE initialise_MMAS
	! Aaron Zecchin, April 2002
	! initialises all MMAS parameters
	! INPUT: input file (UNIT = 15), ant_graph
	! OUTPUT: variables within "para_mmas"

	USE ant_graph
	USE para_mmas

	CHARACTER :: dummy
    integer :: l,k,m
    
	open(unit=44, file="para_type.txt", status="unknown")
	READ(44,*) alpha
	READ(44,*) beta
	READ(44,*) rho
	READ(44,*) q
	READ(44,*) delta
	READ(44,*) pbest
	READ(44,*) freq_g

	! allocating sizes to iteration/local and global best ant types
    allocate(best_g%tree(n_tree))
    allocate(best_i%tree(n_tree))
	do l = 1, n_tree
        allocate(best_g%tree(l)%season(n_sea))
        allocate(best_i%tree(l)%season(n_sea))
        do m = 1, n_sea
            allocate(best_g%tree(l)%season(m)%crop(n_crop(m)))
            allocate(best_i%tree(l)%season(m)%crop(n_crop(m)))
            allocate(best_g%tree(l)%season(m)%dec_crop(max_path))
            allocate(best_i%tree(l)%season(m)%dec_crop(max_path))
            do k = 1, n_crop(m)            
			    allocate(best_g%tree(l)%season(m)%crop(k)%dec_water(max_path))			
			    allocate(best_i%tree(l)%season(m)%crop(k)%dec_water(max_path))
            end do
        end do
	end do
	
END SUBROUTINE initialise_MMAS

!******************************************************************************************
subroutine cost_details

	USE water_model
	USE ACO_input
	USE ant_graph
  
	integer :: i,j,k,l,r,h
	CHARACTER(5) ::dummy 
	real(8), allocatable, dimension(:)  :: interval
    real(8), allocatable, dimension(:) :: y
    real(8), allocatable, dimension(:) :: x
    real(8) :: tvar !temporary variable
    CHARACTER(20) :: cropname   !read name of crop

	OPEN(unit=221, file='input_reservoir.txt', status="unknown")
	OPEN(unit=223, file='input_crop.txt', status="unknown")

	!Reading in reservoir data
	READ(221,*)
	READ(221,*) S0			!reading in initial storage
	READ(221,*) min_storage	!reading in maximum storage
	READ(221,*) max_storage  !reading in minimum storage
	READ(221,*) c1			!evaporation parameters
	READ(221,*) c2
	READ(221,*)
	READ(221,*)
	READ(221,*) al_yr_res		!reading in maximum release per year
	READ(221,*) summ_res
	READ(221,*) aut_res
	READ(221,*) wint_res
	READ(221,*) spri_res

	!Reading in release data
	read(221,*) min_sup_res
	read(221,*) max_sup_res

	do i=1,n_tree
        !Reading in crop data
        do k = 1, n_sea
            READ(223,*) !tree i/season i
            do j = 1, n_crop(k)
                READ(223,*) cropname
                cropname=trim(cropname)
                if (cropname /= "dryland") then
                    READ(223,*) seasons(k)%aa(j,1),seasons(k)%bb(j,1),seasons(k)%cc(j,1)
                    READ(223,*) seasons(k)%aa(j,2),seasons(k)%bb(j,2),seasons(k)%cc(j,2)
                    READ(223,*) seasons(k)%aa(j,3),seasons(k)%bb(j,3),seasons(k)%cc(j,3)
                end if
            end do
        end do
        READ(223,*)
        READ(223,*) pwater
        
		do j=1,max_path
            do k = 1, n_sea
                allocate(x(n_crop(k)))
                allocate(interval(n_crop(k)))
			    do l=1,n_crop(k)
                    ! dryland
                    if (seasons(k)%name_crop(l) == "dryland") then
                        tree(i)%dec(j)%season(k)%opt_crop(l)%property = 1
                        tree(i)%dec(j)%season(k)%opt_crop(l)%cost = 0            
                        x(l) = 0.0
                        seasons(k)%n_opt_water(l) = 1
                        allocate(tree(i)%dec(j)%season(k)%crop(l)%opt_water(1))
                        tree(i)%dec(j)%season(k)%crop(l)%opt_water(1)%property = 0.0
                        tree(i)%dec(j)%season(k)%crop(l)%opt_water(1)%cost = 0
                    else    ! other crops
                        tree(i)%dec(j)%season(k)%opt_crop(l)%property = l
                        x(l) = int((-seasons(k)%bb(l,1)*seasons(k)%bb(l,2)/&
                            (seasons(k)%cc(l,1)*seasons(k)%cc(l,2)*seasons(k)%cc(l,3)))**(1/(seasons(k)%cc(l,3)-1)))
                        tree(i)%dec(j)%season(k)%opt_crop(l)%cost = (seasons(k)%aa(l,1)*&
                            seasons(k)%aa(l,2) + seasons(k)%bb(l,1)*seasons(k)%bb(l,2)*x(l) + &
                            seasons(k)%cc(l,1)*seasons(k)%cc(l,2)*(x(l)**seasons(k)%cc(l,3)))*&
                            seasons(k)%pcrop(l)-(seasons(k)%pcost(l)+pwater*x(l))

                        seasons(k)%n_opt_water(l) = 150
                        interval(l) = wcap
                        allocate(y(seasons(k)%n_opt_water(l)))
                        allocate(tree(i)%dec(j)%season(k)%crop(l)%opt_water(seasons(k)%n_opt_water(l)))
				        do r=1,seasons(k)%n_opt_water(l)
                            y(r) = (r-1)*interval(l)
                            tree(i)%dec(j)%season(k)%crop(l)%opt_water(r)%property = y(r)
                            tvar = (seasons(k)%aa(l,1)*seasons(k)%aa(l,2) + seasons(k)%bb(l,1)*&
                                seasons(k)%bb(l,2)*y(r) + seasons(k)%cc(l,1)*seasons(k)%cc(l,2)&
                                *(y(r)**seasons(k)%cc(l,3)))*seasons(k)%pcrop(l)-&
                                (seasons(k)%pcost(l)+pwater*y(r))
                            if (tvar > 0) then
                                tree(i)%dec(j)%season(k)%crop(l)%opt_water(r)%cost = tvar
                            else 
                                tree(i)%dec(j)%season(k)%crop(l)%opt_water(r)%cost = 0
                            end if
                        end do
                        deallocate(y)
                    end if
                end do
                deallocate(x)
                deallocate(interval)
            end do
        end do
    end do

	CLOSE(221)
	CLOSE(223)
 
END SUBROUTINE cost_details
