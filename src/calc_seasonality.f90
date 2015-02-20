MODULE helpFunction
implicit none

contains

function which1(boolarray) result(indexarray1)
!returns index of the FIRST element that is TRUE in the input array
!if there are more, issue warning, if enabled
implicit none
logical,dimension(:),intent(in):: boolarray
integer,dimension(max(1,count(boolarray))):: indexarray
integer:: indexarray1
integer:: i
	if (count(boolarray) .eq. 0) then
	  indexarray(:)= 0
	else
	  indexarray(:)= pack((/(i,i=1,size(boolarray))/),boolarray)
	end if
	
	indexarray1=indexarray(1)

end function which1


END MODULE helpFunction






SUBROUTINE calc_seasonality2(subbas_id, year, julian_day ,seasonality_in, nv, support_values, seasonality_out)
!replaces calc_seasonality
!compute seasonality (value of current parameter for current timestep and subbasin) by interpolation between node_n and node_n+1

use helpFunction
!use utils_h
implicit none
		
	
INTEGER, INTENT(IN) :: subbas_id, year, julian_day, nv
INTEGER, DIMENSION(:,:), ALLOCATABLE :: seasonality_array
INTEGER, DIMENSION(nv), INTENT(IN) :: seasonality_in
REAL(8), DIMENSION(4), INTENT(IN) :: support_values !real values for 4 DOYs to be interpolated

!real, target :: calc_seasonality2(size(support_values,dim=1))    !return value: a single value for each class (e.g. vegetation)
!Error	in readhymo:	 error #6678: When the target is an expression it must deliver a pointer result.	E:\till\uni\wasa\wasa_svn_comp\Hillslope\readhymo.f90	1695	

!real, pointer :: calc_seasonality2(size(support_values,dim=1))    !return value: a single value for each class (e.g. vegetation)
!error: ALLOCATABLE or POINTER attribute dictates a deferred-shape-array   [CALC_SEASONALITY2]

real(8) :: seasonality_out    !return value: a single value for each class (e.g. vegetation)

integer    :: k, search_year
integer :: d        !distance between start node and current day (in days)
integer :: d_nodes        !distance between start node and end_node (in days)
real :: node1_value, node2_value        !parameter values at nodepoints (start and end-point of interpolation)
integer :: i_node1, i_node2    !indices to relevant nodes in seasonality_array
integer :: doy_node1, doy_node2    !corresponding DOYs
integer :: i_matchrow1, i_matchrow2    !index to matching in seasonality_array
integer :: dy !number of days in the current year (365 or 366 in leap years)


!handling of leap years
IF (MOD((year),4) == 0)  THEN
		dy=366
ELSE
		dy=365
ENDIF

!allocate seasonality_array and create 2D array from input 1D array
allocate(seasonality_array(nv/6,6))
seasonality_array=reshape(seasonality_in, (/ nv/6, 6 /) )


if (size(seasonality_array)==1) then !no seasonality for this parameter
		seasonality_out=support_values(1)    !use single value
		return
end if

!seasonality_out(:) = tiny(seasonality_out(1)) !flag for "not set" - initial value for all entities

!DO irow=1, size(support_values,dim=1) !do loop for all rows (i.e. all vegetation classes)
	!find matching row in seasonality array for current entity

	i_node2 = 0 !not set
	
	i_matchrow1 = &
	which1(	 (seasonality_array(:,1) == subbas_id .OR. seasonality_array(:,1) == -1) .AND. &
				(seasonality_array(:,2) == year      .OR. seasonality_array(:,2) == -1))

	if (i_matchrow1 == 0) stop 'Could not find a row in seasonality_array matching subbas_id and year!'  !no matching row found
	i_matchrow2 = i_matchrow1    !default: other node is also in the same year (row)
				
	do k=3,6
		if (seasonality_array(i_matchrow1, k) > julian_day) exit
	end do

				i_node1 = k - 3
				

	if (i_node1 == 0) then !current DOY is BEFORE first node, lookup other node in previous year
		search_year = -1 !search in the previous year for the other node
	elseif (i_node1 == 4) then !current DOY is AFTER last node, lookup other node in next year
		search_year =  1 !search in the next year for the other node
	else
		search_year = 0 !other node is still in the same year
	end if

	if (search_year /=0) then !search in the other year for the other node (interpolation over year break)
		i_matchrow2 = &
			which1(	 (seasonality_array(:,1) == subbas_id .OR. seasonality_array(:,1) == -1) .AND. &
						(seasonality_array(:,2) == year+search_year    .OR. seasonality_array(:,2) == -1))
				
	end if
			
	if (i_node2 == 0) i_node2 = MOD(i_node1,4) + 1   !only modify i_node2, if it has not been set before
	
	
	if (i_matchrow2 == 0) then  !no matching row found...
			if (search_year == 1) then 
				i_node1 = 4  !extrapolate last value
			else
					i_node1 = 1	!extrapolate first value
			end if
			seasonality_out = support_values(i_node1)
			return
	end if
	
	if (i_node1 == 0) then !swap node_1 and node_2 (interpolation over year break)
		k           = i_matchrow2
		i_matchrow2 = i_matchrow1
		i_matchrow1 = k
		i_node1 = 4
	end if
		
	doy_node1 = seasonality_array(i_matchrow1, 2 + i_node1)
	doy_node2 = seasonality_array(i_matchrow2, 2 + i_node2)
	
	if (i_node1 == 4) then 
		if (doy_node1 > 0) doy_node1 = doy_node1 - dy !force a negative value, as we are looking at the previous year
		if (doy_node2 < 0) doy_node2 = doy_node2 + dy !force a positive value, as we are looking at the next year 
				end if 
				
	d       = julian_day - doy_node1       !distance between start node and current day (in days)
	if (d >= dy) d = d - dy !if current day is after all nodes
				d_nodes = doy_node2  - doy_node1       !distance between start node and end_node (in days)

	if (d < 0 .OR. d_nodes < 0) then !error (presumably in input file)
		stop 'Error: Got negative value when calculating distance between start node and current day (or end note)!'
	end if

	node1_value = support_values(i_node1)    !parameter values at nodepoints (start and end-point of interpolation)
	node2_value = support_values(i_node2)
	seasonality_out = node1_value+ (node2_value-node1_value) * real(d)/d_nodes        !linear interpolation between nodes
!END DO  !end loop for all rows (i.e. all vegetation classes

return

END SUBROUTINE calc_seasonality2



