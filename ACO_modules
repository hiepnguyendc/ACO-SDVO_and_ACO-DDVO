!File contains modules that are used to store varaibles

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
module ACO_input
	!Joanna Szemis, October 2010
	! "ACO_input" stores in ACOA inputs
	integer :: local       !at the moment not used
	integer :: rnd_tot     !at the moment not used
	integer :: max_it      !number of iterations
	integer :: aco_type    !ACOA_type
	real(8) :: pbest		  !used to determine tau_min
	real(8) :: tau_0		  !initial phermone level
	real(8) :: zero_cost   !initial cost if level is zero
   
	INTEGER :: seed_ran_1, seed_ran_2, seed_ran_3 ! random number generator seed val
	INTEGER :: seed_ran_4, seed_ran_5
 
end module ACO_input

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
module ant_graph
	! Aaron Zecchin, April 2002 modified by Joanna Szemis, October 2010, modified by Duc Cong Hiep Nguyen, October 2012
	!"ant_graph" module stores all information about the ACO graph
  
	integer :: n_tree						!number of tree
    integer :: n_dpts                       ! number of decision points
    integer, dimension(2) :: n_crop         !number of crops in each seasons
    integer :: n_both                       !number of crops planed in both seasons
    integer :: max_path                     !number of time steps
    integer :: n_sea                        !number of seasons
    real(8) :: pwater                       !cost of water
    real(8) :: wcap                         !capacity of irrigation system
    
    integer :: bstatus, wbstatus            ! check a both-season crop is selected once at a sub-area
    
    real(8) :: maxA_region                  !maximum area of irrigation
    real(8), dimension(2) :: sea_max_area   !maximum area of each season
    real(8), dimension(2) :: sea_min_area   !minimum area of each season
    
    integer :: unit_area
    integer, allocatable, dimension(:) :: array_areas		!values of sub-areas

    !used to contain the penalty values if the constraints are uncovered
    real(8) :: pen_mincrop, pen_maxcrop, pen_maxsea, pen_wuse    
    
    type sea_details
        !crop production function
		real(8), allocatable, dimension(:,:) :: aa, bb, cc      !cost
        character(20), allocatable, dimension (:) :: name_crop	!name of crop
        integer, allocatable, dimension(:) :: wuse_crop         !make sure the same crop having the same water use
        integer, allocatable, dimension(:) :: n_opt_water		!number of magnitude options for reservoir
        integer, allocatable, dimension(:) :: ws_sea		    !to know whether a crop is planted in winter or summer season
        integer, allocatable, dimension(:) :: bsea  		    !check whether a crop is planted in both season
        
        real(8), allocatable, dimension(:) :: max_crop_area     !maximum area of each area
        real(8), allocatable, dimension(:) :: min_crop_area     !minimum area of each area
        
        real(8), allocatable, dimension(:) :: pcrop             !price of crop
        real(8), allocatable, dimension(:) :: pcost             !cost of crop
    end type sea_details

    type(sea_details), allocatable, dimension (:) :: seasons	!properties of seasons
   
	type option_details
		real(8) :: property			  !stores costs
		real(8) :: heu,prob,up        !heuritic values, probability, update??
		real(8) :: tau				  !pheromone value
		real(8) :: cost				  !cost
	end type option_details

	type crop_details
		type(option_details), allocatable, dimension (:) :: opt_water	!management option for crop
		integer :: max_opt_water                   			            !number of management options for crop       
	end type crop_details

	type season_details
		type(crop_details), allocatable, dimension (:) :: crop		    !number of crops
        type(option_details), allocatable, dimension (:) :: opt_crop	!option for crop
        integer :: max_opt_crop   			                            !number of options for crop
    end type
    
    type dec_details
		type(season_details), allocatable, dimension (:) :: season		!number of crops
	end type
       
	type tree_details 
		type(dec_details), allocatable, dimension (:) :: dec   !number of decision points
    end type
 
	type(tree_details), allocatable, dimension (:) :: tree		!number of decision tree graphs
  
End module ant_graph

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
module ant_colony
	! Aaron Zecchin, April 2002 altered by Joanna Szemis, October 2010, modified by Duc Cong Hiep Nguyen, October 2012
	! "ant_colony" module stores all ant colony data
  
	integer :: max_ant !number of ants
    
	type crop_type
		integer, allocatable, dimension(:) :: dec_water         ! the array of decisions the ant has selected for irrigated amount
		real(8), allocatable, dimension(:) :: random_water  	! random number with which the decision was made
		real(8), allocatable, dimension(:) :: net_return        !yield of crop
        real(8), allocatable, dimension(:) :: water_use         !water allocated to crop
        real(8), allocatable, dimension(:) :: area_planted      !area allocated to crop
        real(8) :: max_area                                     !area allocated to crop
        real(8) :: area_accumulated                             !accumulated area of each crop for each tree
    end type crop_type
    
    type season_type
        type(crop_type), allocatable, dimension (:) :: crop
        integer, allocatable, dimension(:) :: dec_crop 		! the array of decisions the ant has selected for crops
		real(8), allocatable, dimension(:) :: random_crop 	! random number with which the decision was made
        integer, allocatable, dimension(:) :: check_crop    !check a crop is selected or not, to make decision for selecting water
    end type season_type

    type tree_type
		type(season_type), allocatable, dimension (:) :: season		!number of crops
	end type tree_type
    
	type ant_type
        type(tree_type), allocatable, dimension (:) :: tree
        real(8) :: water_accumulated			!accumulated area of each crop for each tree
        real(8), dimension(2) :: sea_cur_area     !current area of each season without dryland
        real(8), dimension(2) :: sea_cur_dryarea     !current area of each season with dryland
		real(8) :: val,cost,pen_cost,pen		!stores the final obj function, cost, penality costs and penalty
	end type ant_type
  
	type(ant_type), allocatable, dimension(:) :: sol,ant					!ant and solution
 
end module 

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
module para_as
	! Aaron Zecchin, April 2002
	! stores parameters for the ant system 

	real(8) :: alpha  ! pheromone importance factor
	real(8) :: beta   ! visibility importance factor
	real(8) :: rho    ! pheromone persistance
	real(8) :: q      ! pheromone reward factor

end module para_as  

! ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
MODULE para_mmas
	! Aaron Zecchin, April 2002
	! stores parameters for max-min ant system 
      
	USE ant_colony

	INTEGER :: freq_g					! frequency of global best updating          
	INTEGER :: count_g					! counter for global best updating
	REAL(8) :: alpha					! pheromone importance factor
	REAL(8) :: beta						! visibility importance factor
	REAL(8) :: rho						! pheromone persistance factor
	REAL(8) :: q						! pheromone reward factor
	REAL(8) :: delta					! pheromone trail smoothing factor (PTS)
	REAL(8):: pbest						! used to determine tau_min
	REAL(8) :: tau_max					! upper pheromone bound
	REAL(8) :: tau_min_crop          	! lower pheromone bound
	REAL(8) :: tau_min_water         	! lower pheromone bound
	TYPE(ant_type) :: best_g			! global best ant
	TYPE(ant_type) :: best_i			! iteration best ant 

END MODULE para_mmas

! ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
module ant_store
	! Aaron Zecchin, 19 July 2002, modified by Joanna Szemis, Oct 2010, modified by Duc Cong Hiep Nguyen, October 2012
	! Stores top "max_store" ant results

	integer :: max_store                              ! number of best performing ants that are to be stored

	type crop_store_type
		integer,allocatable, dimension(:) :: dec_water      	! the array of decisions the ant has selected for crops
		real(8), allocatable, dimension(:) :: net_return		!yield of crop	
        real(8), allocatable, dimension(:) :: water_use      	!water allocated to crop
        real(8), allocatable, dimension(:) :: area_planted		!area allocated to crop

    end type crop_store_type
   
    type season_store_type
        type(crop_store_type), allocatable, dimension (:) :: crop
        integer,allocatable, dimension(:) :: dec_crop   	! the array of decisions the ant has selected for crops
    end type season_store_type
    
    type tree_store_type
		type(season_store_type), allocatable, dimension (:) :: season		!number of crops
	end type tree_store_type
    
	type store_type
        type(tree_store_type), allocatable, dimension (:) :: tree
		integer :: evaluation_num							! number at which the value1 was found
		real(8) :: val										! objective function value1 of the ants path
		real(8) :: pen_cost									! penalty cost
		real(8) :: pen										! penalty value1
		real(8) :: cost										! actual cost     
	end type store_type

	type(store_type), allocatable, dimension(:) ::  store    

end module ant_store 
   
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    
MODULE ant_print_control
	! Aaron Zecchin, 25 July 2002
	! Stores printing settings for ACO
	integer :: num,num_2
	integer :: print_list,print_num,print_sum   
END MODULE
    
!::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!Module containing random number generator 
MODULE r_num
	integer :: defaultsd != 4357 !1121  !2286 !1411
	! Period parameters
	integer, parameter :: N = 624, N1 = N + 1
	! the array for the state vector
	integer, save, dimension(0:N-1) :: mt
	integer, save                   :: mti = N1
  
	contains    
		subroutine sgrnd(seed)
			implicit none
			! setting initial seeds to mt[N] using the generator Line 25 of Table 1 in
			! [KNUTH 1981, The Art of Computer Programming, Vol. 2 (2nd Ed.), pp102]

			integer, intent(in) :: seed
			mt(0) = iand(seed,-1)
			do mti=1,N-1
				mt(mti) = iand(69069 * mt(mti-1),-1)
			end do
			return
		end subroutine sgrnd

		!Random number generator
		real(8) function grnd()
			implicit integer(a-z)
			! Period parameters
			integer, parameter :: M = 397, MATA  = -1727483681		! constant vector a
			integer, parameter :: LMASK =  2147483647				! least significant r bits
			integer, parameter :: UMASK = -LMASK - 1
			! most significant w-r bits
			! Tempering parameters
			integer, parameter :: TMASKB= -1658038656, TMASKC= -272236544
			dimension mag01(0:1)
			data mag01/0, MATA/
			save mag01
			! mag01(x) = x * MATA for x=0,1
			TSHFTU(y)=ishft(y,-11)
			TSHFTS(y)=ishft(y,7)
			TSHFTT(y)=ishft(y,15)
			TSHFTL(y)=ishft(y,-18)

			if (mti.ge.N) then						!generate N words at one time
				if (mti.eq.N+1) then				!if sgrnd() has not been called,
					call sgrnd( defaultsd )			!a default initial seed is used
				end if
				do kk=0,N-M-1
					y=ior(iand(mt(kk),UMASK),iand(mt(kk+1),LMASK))
					mt(kk)=ieor(ieor(mt(kk+M),ishft(y,-1)),mag01(iand(y,1)))
				end do
				do kk=N-M,N-2
					y=ior(iand(mt(kk),UMASK),iand(mt(kk+1),LMASK))
					mt(kk)=ieor(ieor(mt(kk+(M-N)),ishft(y,-1)),mag01(iand(y,1)))
				end do

				y=ior(iand(mt(N-1),UMASK),iand(mt(0),LMASK))
				mt(N-1)=ieor(ieor(mt(M-1),ishft(y,-1)),mag01(iand(y,1)))
				mti = 0
			end if
			y=mt(mti)
			mti = mti + 1 
			y=ieor(y,TSHFTU(y))
			y=ieor(y,iand(TSHFTS(y),TMASKB))
			y=ieor(y,iand(TSHFTT(y),TMASKC))
			y=ieor(y,TSHFTL(y))
			if(y .lt. 0) then
				grnd=(dble(y)+2.0d0**32)/(2.0d0**32-1.0d0)
			else
				grnd=dble(y)/(2.0d0**32-1.0d0)
			end if
			return
		end function grnd
  
END MODULE r_num
  
 !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
MODULE water_model
	! Joanna Szemis October 2010
	!"water_model" module contains varibles for water simluation model
	real(8) :: cost_matrix1(100,100,100),cost_matrix2(100,100,100) !contains the costs
	REAL(8) :: S0									!inital reservoir storage
	REAL(8) :: al_yr_res                    		!allocation per year
	REAL(8) :: max_storage,min_storage				!max and min reservoir storage
	REAL(8) :: c1,c2								!dam evaporation parameters
	REAL(8) :: wet_evap(12),month(12)				!evaporation and days in each month
	REAL(8) :: summ_res, aut_res, wint_res, spri_res		
	REAL(8) :: summ_irr, aut_irr, wint_irr, spri_irr		
	REAL(8) :: summ_water, aut_water, wint_water, spri_water		
   
	REAL(8), ALLOCATABLE, DIMENSION (:) :: inflows,PET		!inflows and pan evaporation
	REAL(8), ALLOCATABLE, DIMENSION (:) :: s,evap				!reservoir storage and evaporation
	REAL(8), ALLOCATABLE, DIMENSION (:) :: spill,rel		!spill, releasea and final release (spill+release)
	REAL(8), ALLOCATABLE, DIMENSION (:) :: fr,min_f
     
END MODULE
!::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
   
