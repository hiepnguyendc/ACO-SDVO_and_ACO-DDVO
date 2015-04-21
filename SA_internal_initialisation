!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!
! CONTAINS:
! SUBROUTINE SA_internal_initialisation(num_sa)
! SUBROUTINE internal_initialise_sa_as(num_sa)
! SUBROUTINE internal_initialise_sa_mmas(num_sa)
!
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
subroutine SA_internal_initialisation(num_sa)

! Aaron Zecchin, July 30 2002
! Initialises parameter for sensitivity analysis loop
! INPUT: num_sa, aco_type, sensitivity_analysis[sa(sa_num)%para, para_name]
! OUTPUT: ant_graph[tau_0], ant_colony[max_ant], max_ant_main, max_it

    use sensitivity_analysis
    use ant_graph
    use ant_colony
	use ACO_input

    integer :: num_sa

    if(para_name == 'tau_0') then
      tau_0 = sa(num_sa)%para
    else if(para_name == 'max_ant') then
      max_ant = sa(num_sa)%para
      max_it  = max_evaluation / max_ant
      max_ant_main = max_ant
    else
      if(aco_type == 1) call internal_initialise_sa_as(num_sa)
      IF(aco_type == 5) CALL internal_initialise_sa_mmas(num_sa)
    end if    
    
  end subroutine SA_internal_initialisation

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  subroutine internal_initialise_sa_as(num_sa)
    use para_as
    use sensitivity_analysis

    integer :: num_sa

    if(para_name == 'alpha') then 
     alpha = sa(num_sa)%para
    else if (para_name == 'beta') THEN
     beta = sa(num_sa)%para
    else if (para_name == 'rho') THEN
     rho = sa(num_sa)%para
    else if (para_name == 'q') THEN
     q = sa(num_sa)%para
    end if
  
  end subroutine internal_initialise_sa_as
  
  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    SUBROUTINE internal_initialise_sa_mmas(num_sa)

    USE para_mmas
    USE sensitivity_analysis
    USE ant_print_control

    INTEGER :: num_sa

    IF(para_name == 'alpha')THEN 
     alpha = sa(num_sa)%para
    ELSE IF (para_name == 'beta') THEN
     beta = sa(num_sa)%para
    ELSE IF (para_name == 'rho') THEN
     rho = sa(num_sa)%para
	ELSE IF (para_name == 'q') THEN
     q = sa(num_sa)%para
    ELSE IF (para_name == 'delta') THEN
     delta = sa(num_sa)%para
    ELSE IF (para_name == 'pbest') THEN
     pbest = sa(num_sa)%para
    END IF

    END SUBROUTINE internal_initialise_sa_mmas

!::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
