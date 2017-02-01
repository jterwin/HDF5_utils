!
!
!>  \brief real/double precision kinds
module kinds
  integer, parameter :: sp = kind(1.0)
  integer, parameter :: dp = kind(1.0d0)
end module kinds


!>  \brief a set of high level wrapper subroutine for HDF5
!>
!>  \par \b Features:
!>   - opening and closing files
!>   - reading and writing dataset (double)
!>   - use aliases to simply calls (rank)
!>
!>  \todo
!>   - reading and writing (integer, real, character/string) 
!>   - use aliases to simply calls (kind)
!>   - reading/writing attribute
!>   - creating groups
!>   - get_rank, get_dim, get_kind
!>   - get_id
!>   - check with nested
!>   - error checking, stop on error?
!>
!>  \note I might use H5T_STD_F64BE (or H5T_STD_F64LE) instead of H5T_NATIVE_DOUBLE when
!>    creating a dataset. This would make the hdf5 file more portable.
module HDF5_utils
  
  !use, intrinsic :: iso_fortran_env, only: dp=>real64
  use kinds
  use hdf5
  implicit none

  !> \brief generic interface to write a dataset of doubles
  interface hdf_write_dataset_double
     module procedure hdf_write_dataset_double_0
     module procedure hdf_write_dataset_double_1
     module procedure hdf_write_dataset_double_2
     module procedure hdf_write_dataset_double_3
     module procedure hdf_write_dataset_double_4
  end interface hdf_write_dataset_double
  

contains

  !> \brief opens file and return identifier
  !
  !>  \todo
  !>   - use STATUS and ACTION
  !>   - case insentive STATUS and ACTION
  subroutine hdf_open(file_id, filename, STATUS, ACTION)

    integer(HID_T), intent(out) :: file_id            !< HDF5 id of the file
    character(len=*), intent(in) :: filename          !< filename
    character(len=*), optional, intent(in) :: STATUS  !< file status (OLD, NEW, REPLACE)
    character(len=*), optional, intent(in) :: ACTION  !< file action (READ, WRITE, READWRITE)

    integer :: hdferror
    character(len=16) :: status2, action2
    
    write(*,'(A)') "->hdf_open"
    
    ! open hdf5 interface
    call h5open_f(hdferror)
    write(*,'(A20,I0)') "h5open: ", hdferror

    !
    if (present(STATUS)) write(*,'(A,A)') "STATUS = ", STATUS
    if (present(ACTION)) write(*,'(A,A)') "ACTION = ", ACTION

    status2 = 'NEW'
    if (present(STATUS)) status2 = STATUS
    action2 = 'WRITE'
    if (present(STATUS)) action2 = ACTION

    ! open/create hdf5 file
    if (status2 == 'OLD') then
       if (action2 == 'READ') then
          call h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, hdferror)
       elseif ( (action2 == 'WRITE') .or. (action2 == 'READWRITE') ) then
          call h5fopen_f(filename, H5F_ACC_RDWR_F, file_id, hdferror)
       else
          write(*,*) "hdf_open: action = ", action2, " not supported." 
          stop
       end if
    elseif (status2 == 'NEW') then
       call h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, hdferror)
    elseif (status2 == 'REPLACE') then
       call h5fcreate_f(filename, H5F_ACC_EXCL_F, file_id, hdferror)
    else
       write(*,*) "hdf_open: status = ", status2, " not supported." 
       stop
    end if
    
    write(*,'(A20,I0)') "h5fcreate: ", hdferror

  end subroutine hdf_open

  !
  !
  !
  subroutine hdf_close(file_id)

    integer(HID_T), intent(in) :: file_id

    integer :: hdferror

    write(*,'(A)') "->hdf_close"

    call h5fclose_f(file_id, hdferror)
    write(*,'(A20,I0)') "h5fclose: ", hdferror

  end subroutine hdf_close



  !!--------------------------------hdf_write_dataset_double--------------------------------
  
  !
  !
  !
  subroutine hdf_write_dataset_double_0(loc_id, dset_name, data)

    integer(HID_T), intent(in) :: loc_id
    character(len=*), intent(in) :: dset_name
    real(dp), intent(in) :: data

    integer(SIZE_T) :: dims(1)
    integer(HID_T) :: dset_id, dspace_id
    integer :: hdferror

    write(*,'(A)') "->hdf_write_dataset_double_0"

    ! set rank and dims
    dims = (/ 0 /)

    ! create dataspace
    call h5screate_f(H5S_SCALAR_F, dspace_id, hdferror)
    !write(*,'(A20,I0)') "h5screate_simple: ", hdferror

    ! create dataset
    call h5dcreate_f(loc_id, dset_name, H5T_NATIVE_DOUBLE, dspace_id, dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dcreate: ", hdferror

    ! write dataset
    call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, data, dims, hdferror)
    !write(*,'(A20,I0)') "h5dwrite: ", hdferror
    
    ! close all id's
    call h5dclose_f(dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dclose: ", hdferror
    call h5sclose_f(dspace_id, hdferror)
    !write(*,'(A20,I0)') "h5sclose: ", hdferror

  end subroutine hdf_write_dataset_double_0

  !
  !
  !
  subroutine hdf_write_dataset_double_1(loc_id, dset_name, data)

    integer(HID_T), intent(in) :: loc_id
    character(len=*), intent(in) :: dset_name
    real(dp), intent(in) :: data(:)

    integer :: rank
    integer(SIZE_T) :: dims(1)
    integer(HID_T) :: dset_id, dspace_id
    integer :: hdferror

    write(*,'(A)') "->hdf_write_dataset_double_1"

    ! set rank and dims
    rank = 1
    dims = shape(data, KIND=HID_T)

    ! create dataspace
    call h5screate_simple_f(rank, dims, dspace_id, hdferror)
    !write(*,'(A20,I0)') "h5screate_simple: ", hdferror

    ! create dataset
    call h5dcreate_f(loc_id, dset_name, H5T_NATIVE_DOUBLE, dspace_id, dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dcreate: ", hdferror

    ! write dataset
    call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, data, dims, hdferror)
    !write(*,'(A20,I0)') "h5dwrite: ", hdferror
    
    ! close all id's
    call h5dclose_f(dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dclose: ", hdferror
    call h5sclose_f(dspace_id, hdferror)
    !write(*,'(A20,I0)') "h5sclose: ", hdferror

  end subroutine hdf_write_dataset_double_1

  
  !
  !
  !
  subroutine hdf_write_dataset_double_2(loc_id, dset_name, data)

    integer(HID_T), intent(in) :: loc_id
    character(len=*), intent(in) :: dset_name
    real(dp), intent(in) :: data(:,:)

    integer :: rank
    integer(SIZE_T) :: dims(2)
    integer(HID_T) :: dset_id, dspace_id
    integer :: hdferror

    write(*,'(A)') "->hdf_write_dataset_double_2"

    ! set rank and dims
    rank = 2
    dims = shape(data, KIND=HID_T)

    ! create dataspace
    call h5screate_simple_f(rank, dims, dspace_id, hdferror)
    !write(*,'(A20,I0)') "h5screate_simple: ", hdferror

    ! create dataset
    call h5dcreate_f(loc_id, dset_name, H5T_NATIVE_DOUBLE, dspace_id, dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dcreate: ", hdferror

    ! write dataset
    call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, data, dims, hdferror)
    !write(*,'(A20,I0)') "h5dwrite: ", hdferror
    
    ! close all id's
    call h5dclose_f(dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dclose: ", hdferror
    call h5sclose_f(dspace_id, hdferror)
    !write(*,'(A20,I0)') "h5sclose: ", hdferror

  end subroutine hdf_write_dataset_double_2


    !
  !
  !
  subroutine hdf_write_dataset_double_3(loc_id, dset_name, data)

    integer(HID_T), intent(in) :: loc_id
    character(len=*), intent(in) :: dset_name
    real(dp), intent(in) :: data(:,:,:)

    integer :: rank
    integer(SIZE_T) :: dims(3)
    integer(HID_T) :: dset_id, dspace_id
    integer :: hdferror

    write(*,'(A)') "->hdf_write_dataset_double_3"

    ! set rank and dims
    rank = 3
    dims = shape(data, KIND=HID_T)

    ! create dataspace
    call h5screate_simple_f(rank, dims, dspace_id, hdferror)
    !write(*,'(A20,I0)') "h5screate_simple: ", hdferror

    ! create dataset
    call h5dcreate_f(loc_id, dset_name, H5T_NATIVE_DOUBLE, dspace_id, dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dcreate: ", hdferror

    ! write dataset
    call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, data, dims, hdferror)
    !write(*,'(A20,I0)') "h5dwrite: ", hdferror
    
    ! close all id's
    call h5dclose_f(dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dclose: ", hdferror
    call h5sclose_f(dspace_id, hdferror)
    !write(*,'(A20,I0)') "h5sclose: ", hdferror

  end subroutine hdf_write_dataset_double_3


    !
  !
  !
  subroutine hdf_write_dataset_double_4(loc_id, dset_name, data)

    integer(HID_T), intent(in) :: loc_id
    character(len=*), intent(in) :: dset_name
    real(dp), intent(in) :: data(:,:,:,:)

    integer :: rank
    integer(SIZE_T) :: dims(4)
    integer(HID_T) :: dset_id, dspace_id
    integer :: hdferror

    write(*,'(A)') "->hdf_write_dataset_double_4"

    ! set rank and dims
    rank = 4
    dims = shape(data, KIND=HID_T)

    ! create dataspace
    call h5screate_simple_f(rank, dims, dspace_id, hdferror)
    !write(*,'(A20,I0)') "h5screate_simple: ", hdferror

    ! create dataset
    call h5dcreate_f(loc_id, dset_name, H5T_NATIVE_DOUBLE, dspace_id, dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dcreate: ", hdferror

    ! write dataset
    call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, data, dims, hdferror)
    !write(*,'(A20,I0)') "h5dwrite: ", hdferror
    
    ! close all id's
    call h5dclose_f(dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dclose: ", hdferror
    call h5sclose_f(dspace_id, hdferror)
    !write(*,'(A20,I0)') "h5sclose: ", hdferror

  end subroutine hdf_write_dataset_double_4


  !!--------------------------------hdf_read_dataset_double--------------------------------

      !
  !
  !
  subroutine hdf_read_dataset_double_1(loc_id, dset_name, data)

    integer(HID_T), intent(in) :: loc_id
    character(len=*), intent(in) :: dset_name
    real(dp), intent(out) :: data(:)

    integer :: rank
    integer(SIZE_T) :: dims(1)
    integer(HID_T) :: dset_id, dspace_id
    integer :: hdferror

    write(*,'(A)') "->hdf_read_dataset_double_1"

    ! set rank and dims
    rank = 1
    dims = shape(data, KIND=HID_T)

    ! open dataset
    call h5dopen_f(loc_id, dset_name, dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dcreate: ", hdferror

    ! write dataset
    call h5dread_f(dset_id, H5T_NATIVE_DOUBLE, data, dims, hdferror)
    !write(*,'(A20,I0)') "h5dwrite: ", hdferror
    
    ! close all id's
    call h5dclose_f(dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dclose: ", hdferror

  end subroutine hdf_read_dataset_double_1

end module HDF5_utils
