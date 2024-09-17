program fortran_mpi

    use mpi

    implicit none

    integer :: ierror
    integer :: rank, comm_size
    integer :: timer_id = 1
    integer :: sec, nsec
    integer :: n_elements = 100000
    real, dimension(:), allocatable :: array
    real, dimension(:), pointer :: my_array

    call initalize_mpi(comm_size, rank, ierror)
    if (ierror /= MPI_SUCCESS) then
        write(0, *) "ERROR: Error when calling intialize_mpi: ", ierror
        stop
    end if

    if (rank == 0) then
        call start_timer(timer_id)
    end if

    if (rank == 0) then
        call init_array(rank, n_elements, array)
    end if

    call distribute_array(rank, comm_size, array, my_array)

    call do_work(my_array)

    if (rank == 0) then
        call end_timer(timer_id, sec, nsec)
        write(0,*) "Time Taken: ", sec, nsec
    end if

    call MPI_finalize(ierror)

contains

    subroutine initalize_mpi(comm_size, rank, ierror)

        use mpi

        implicit none
        
        integer, intent(out) :: comm_size
        integer, intent(out) :: rank
        integer, intent(out) :: ierror

        call mpi_init(ierror)
        if (ierror /= MPI_SUCCESS) then
            write(0, *) "ERROR: Failed to call: `mpi_init` successfully!"
            comm_size = -1
            rank = -1
            return
        end if

        call mpi_comm_size(MPI_COMM_WORLD, comm_size, ierror)
        if (ierror /= MPI_SUCCESS) then
            write(0, *) "ERROR: Failed to call: `mpi_comm_size` successfully!"
            comm_size = -1
            rank = -1
            return
        end if

        call mpi_comm_rank(MPI_COMM_WORLD, rank, ierror)
        if (ierror /= MPI_SUCCESS) then
            write(0, *) "ERROR: Failed to call: `mpi_comm_rank` successfully!"
            comm_size = -1
            rank = -1
            return
        end if

    end subroutine initalize_mpi

    subroutine init_array(rank, n_elements, array)

        implicit none

        integer, intent(in) :: rank
        integer, intent(in) :: n_elements
        real, dimension(:), allocatable :: array

        integer :: i

        if (rank /= 0) then
            return
        end if

        allocate(array(n_elements))

        do i = 1, n_elements, 1
            array(i) = i
        end do

    end subroutine init_array 

    subroutine distribute_array(rank, comm_size, array, my_array)

        implicit none

        integer, intent(in) :: rank
        integer, intent(in) :: comm_size
        real, intent(inout), dimension(:), allocatable :: array
        real, intent(out), dimension(:), pointer :: my_array

        integer, dimension(:), allocatable :: send_counts
        integer, dimension(:), allocatable :: displacements
        
        integer :: per_each_process
        integer :: remainder
        integer :: n_elems_for_this_process
        integer :: i 
        
        ! Get the send_counts for each processor
        per_each_process = n_elements / comm_size
        remainder = mod(n_elements, comm_size)

        allocate(send_counts(comm_size))
        allocate(displacements(comm_size))

        do i = 1, comm_size, 1
            if (i == comm_size) then
                n_elems_for_this_process = per_each_process + remainder
                continue
            else 
                n_elems_for_this_process = per_each_process
            end if

            send_counts(i) = n_elems_for_this_process
            displacements(i) = ((i - 1) * per_each_process)
        end do

        allocate(my_array(send_counts(rank + 1)))

        call MPI_ScatterV(array, send_counts, displacements, &
            MPI_REAL, my_array, send_counts(rank + 1), MPI_REAL, 0, MPI_COMM_WORLD, ierror)

    end subroutine distribute_array

    subroutine do_work(array)
        
        implicit none

        real, intent(inout), dimension(:), pointer :: array
        integer :: i, j

        ! Perform some slow operation (n^2)

        !$OMP DO
        do i = 1, size(array), 1
           do j = 1, size(array), 1
            array(i) = array(i) + sqrt(array(i)) + sqrt(array(j))
           end do
        end do
        !$OMP END DO

    end subroutine do_work

    subroutine start_timer(timer_id)

        use iso_c_binding, only : c_int

        integer, intent(in) :: timer_id

        interface
            subroutine timer_start(timer_id) bind(C)
               use iso_c_binding, only : c_int
               integer (c_int), intent(in), value :: timer_id
            end subroutine timer_start
        end interface
     
        integer (c_int) :: c_timer_id

        c_timer_id = timer_id

        call timer_start(c_timer_id)

    end subroutine start_timer


    subroutine end_timer(timer_id, sec, nsec)
        use iso_c_binding, only : c_int

        integer, intent(in) :: timer_id
        integer, intent(out) :: sec, nsec
     
        interface
            subroutine timer_stop(timer_id, sec, nsec) bind(C)
               use iso_c_binding, only : c_int
               integer (c_int), intent(in), value :: timer_id
               integer (c_int), intent(out) :: sec, nsec
            end subroutine timer_stop
        end interface
     
        integer (c_int) :: c_timer_id, c_sec, c_nsec 

        c_timer_id = timer_id
        call timer_stop(c_timer_id, c_sec, c_nsec)

        sec = c_sec
        nsec = c_nsec
        
    end subroutine end_timer

end program
