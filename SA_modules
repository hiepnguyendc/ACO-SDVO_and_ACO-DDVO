
module sensitivity_analysis

! Aaron Zecchin, April 2002, Joanna Szemis, Ocotber 2010, modified by Duc Cong Hiep Nguyen, October 2012
! this module stores the results for all the random loop value1s for all the sensitivity analysis (sa) loops

  integer :: max_sa                                         ! max_sa is the number of parameter value1s
  integer :: max_rand                                       ! no. of random no. loops
  integer :: max_evaluation                                 ! max_evaluations is the maximum number of evaluations
  character (LEN = 15) :: para_name                         ! name of parameter that is varied  

  type sa_crop_type
    integer,allocatable, dimension(:) :: dec_water  		! the array of decisions the ant has selected for crops
	real(8), allocatable, dimension(:) :: prop_water  	! random number with which the decision was made   
	real(8), allocatable, dimension(:) :: net_return			!yield of crop
    real(8), allocatable, dimension(:) :: water_use         		!water allocated to crop
    real(8), allocatable, dimension(:) :: area_planted				!area allocated to crop
  end type sa_crop_type 

  type sa_season_type
    integer,allocatable, dimension(:) :: dec_crop 		! the array of decisions the ant has selected for crops
	real(8), allocatable, dimension(:) :: prop_crop 	! random number with which the decision was made   
    type(sa_crop_type), allocatable, dimension (:) :: crop
  end type sa_season_type
  
  type sa_tree_type
    type(sa_season_type), allocatable, dimension (:) :: season		!number of crops
  end type sa_tree_type
 
  type random_loop_type                                     ! stores details about best performing ant in each run
    type(sa_tree_type), allocatable, dimension (:) :: tree
    integer :: evaluation_num                               ! first evaluation no. minimum value1 was found
    real(8) :: val												! as in "ant_type"
	real(8) :: pen_cost                                         ! penalty cost
	real(8) :: cost                                             ! actual cost
  end type random_loop_type

  type sa_loop_type
    type(random_loop_type), allocatable, dimension(:) :: rand
    real(8) :: para
  end type sa_loop_type

  type(sa_loop_type), allocatable, dimension(:) :: sa

end module sensitivity_analysis
