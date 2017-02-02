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
!>   - reading and writing dataset (integer, double)
!>     - uses generic interface to switch on rank and kind
!>   - writing/reading attributes (integer, double, string)
!>     - uses generic interfaces to switch on rank and kind 
!>
!>  \todo
!>   - reading and writing ( real, character/string)
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

  !>  \brief generic interface to write a dataset
  !>  \param[in] loc_d     local id in file
  !>  \param[in] dset_name name of dataset
  !>  \param[in] data      data array to be written
  interface hdf_write_dataset
     module procedure hdf_write_dataset_integer_0
     module procedure hdf_write_dataset_integer_1
     module procedure hdf_write_dataset_integer_2
     module procedure hdf_write_dataset_integer_3
     module procedure hdf_write_dataset_integer_4
     module procedure hdf_write_dataset_integer_5
     module procedure hdf_write_dataset_integer_6
     module procedure hdf_write_dataset_double_0
     module procedure hdf_write_dataset_double_1
     module procedure hdf_write_dataset_double_2
     module procedure hdf_write_dataset_double_3
     module procedure hdf_write_dataset_double_4
     module procedure hdf_write_dataset_double_5
     module procedure hdf_write_dataset_double_6
  end interface hdf_write_dataset
  
  !>  \brief generic interface to write a dataset of integers
  !>  \param[in] loc_d     local id in file
  !>  \param[in] dset_name name of dataset
  !>  \param[in] data      data array to be written
  interface hdf_write_dataset_integer
     module procedure hdf_write_dataset_integer_0
     module procedure hdf_write_dataset_integer_1
     module procedure hdf_write_dataset_integer_2
     module procedure hdf_write_dataset_integer_3
     module procedure hdf_write_dataset_integer_4
     module procedure hdf_write_dataset_integer_5
     module procedure hdf_write_dataset_integer_6
  end interface hdf_write_dataset_integer
  
  !>  \brief generic interface to write a dataset of doubles
  !>  \param[in] loc_d     local id in file
  !>  \param[in] dset_name name of dataset
  !>  \param[in] data      data array to be written
  interface hdf_write_dataset_double
     module procedure hdf_write_dataset_double_0
     module procedure hdf_write_dataset_double_1
     module procedure hdf_write_dataset_double_2
     module procedure hdf_write_dataset_double_3
     module procedure hdf_write_dataset_double_4
     module procedure hdf_write_dataset_double_5
     module procedure hdf_write_dataset_double_6
  end interface hdf_write_dataset_double


  !> \brief generic interface to read a dataset of doubles
  !>  \param[in]  loc_d     local id in file
  !>  \param[in]  dset_name name of dataset
  !>  \param[out] data data array to be read
  interface hdf_read_dataset
     module procedure hdf_read_dataset_integer_0
     module procedure hdf_read_dataset_integer_1
     module procedure hdf_read_dataset_integer_2
     module procedure hdf_read_dataset_integer_3
     module procedure hdf_read_dataset_integer_4
     module procedure hdf_read_dataset_integer_5
     module procedure hdf_read_dataset_integer_6
     module procedure hdf_read_dataset_double_0
     module procedure hdf_read_dataset_double_1
     module procedure hdf_read_dataset_double_2
     module procedure hdf_read_dataset_double_3
     module procedure hdf_read_dataset_double_4
     module procedure hdf_read_dataset_double_5
     module procedure hdf_read_dataset_double_6
  end interface hdf_read_dataset

  !> \brief generic interface to read a dataset of doubles
  !>  \param[in]  loc_d     local id in file
  !>  \param[in]  dset_name name of dataset
  !>  \param[out] data data array to be read
  interface hdf_read_dataset_integer
     module procedure hdf_read_dataset_integer_0
     module procedure hdf_read_dataset_integer_1
     module procedure hdf_read_dataset_integer_2
     module procedure hdf_read_dataset_integer_3
     module procedure hdf_read_dataset_integer_4
     module procedure hdf_read_dataset_integer_5
     module procedure hdf_read_dataset_integer_6
  end interface hdf_read_dataset_integer

  !> \brief generic interface to read a dataset of doubles
  !>  \param[in]  loc_d     local id in file
  !>  \param[in]  dset_name name of dataset
  !>  \param[out] data data array to be read
  interface hdf_read_dataset_double
     module procedure hdf_read_dataset_double_0
     module procedure hdf_read_dataset_double_1
     module procedure hdf_read_dataset_double_2
     module procedure hdf_read_dataset_double_3
     module procedure hdf_read_dataset_double_4
     module procedure hdf_read_dataset_double_5
     module procedure hdf_read_dataset_double_6
  end interface hdf_read_dataset_double

  !>  \brief generic interface to write attribute
  !>  \param[in] loc_id    local id in file
  !>  \param[in] obj_name  name of object to be attached to (if left blank, just use loc_id)
  !>  \param[in] attr_name name of attribute to be added
  !>  \param[in] data      attribute data to be written
  interface hdf_write_attr
     module procedure hdf_write_attr_string
     module procedure hdf_write_attr_integer_0
     module procedure hdf_write_attr_integer_1
     module procedure hdf_write_attr_double_0
     module procedure hdf_write_attr_double_1
  end interface hdf_write_attr

  !> \brief generic interface to write attribute of integers
  interface hdf_write_attr_integer
     module procedure hdf_write_attr_integer_0
     module procedure hdf_write_attr_integer_1
  end interface hdf_write_attr_integer
  
  !> \brief generic interface to write attribute of doubles
  interface hdf_write_attr_double
     module procedure hdf_write_attr_double_0
     module procedure hdf_write_attr_double_1
  end interface hdf_write_attr_double

  !>  \brief generic interface to read attribute
  !>  \param[in] loc_id    local id in file
  !>  \param[in] obj_name  name of object to be attached to (if left blank, just use loc_id)
  !>  \param[in] attr_name name of attribute to be added
  !>  \param[in] data      attribute data to be written
  interface hdf_read_attr
     module procedure hdf_read_attr_string
     module procedure hdf_read_attr_double_0
     module procedure hdf_read_attr_double_1
  end interface hdf_read_attr
  
     

contains

  
  !>  \brief opens file and return identifier
  !>
  !>  | STATUS  | ACTION    | Description                          |
  !>  | :-----: | :-------: | :----------------------------------- |
  !>  | NEW     | na        | calls h5fcreate with H5F_ACC_TRUNC_F |
  !>  | REPLACE | na        | calls h5fcreate with H5F_ACC_EXCL_F  |       
  !>  | OLD     | READ      | calls h5fopen with H5F_ACC_RDONLY_F  |
  !>  | OLD     | WRITE     | calls h5fopen with H5F_ACC_RDWR_F    |
  !>  | OLD     | READWRITE | calls h5fopen with H5F_ACC_RDWR_F    |
  !>
  !>  \todo
  !>   - case insentive STATUS and ACTION
  !>   - delete file for REPLACE case
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

    ! set defaults
    status2 = 'NEW'
    if (present(STATUS)) status2 = STATUS
    action2 = 'READWRITE'
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


  !>  \brief closes a hdf5 file
  subroutine hdf_close(file_id)

    integer(HID_T), intent(in) :: file_id  !< file id to be closed

    integer :: hdferror

    write(*,'(A)') "->hdf_close"

    call h5fclose_f(file_id, hdferror)
    write(*,'(A20,I0)') "h5fclose: ", hdferror

  end subroutine hdf_close


  !!----------------------------------------------------------------------------------------
  !!--------------------------------hdf_write_dataset_double--------------------------------
  !!----------------------------------------------------------------------------------------

  !>  \brief writes a scalar to an hdf5 file
  subroutine hdf_write_dataset_double_0(loc_id, dset_name, data)

    integer(HID_T), intent(in) :: loc_id        !< local id in file
    character(len=*), intent(in) :: dset_name   !< name of dataset
    real(dp), intent(in) :: data                !< data to be written

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

  !>  \brief writes a 1d array to an hdf5 file
  subroutine hdf_write_dataset_double_1(loc_id, dset_name, data)

    integer(HID_T), intent(in) :: loc_id        !< local id in file
    character(len=*), intent(in) :: dset_name   !< name of dataset
    real(dp), intent(in) :: data(:)             !< data to be written

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

  !>  \brief writes a 2d array to an hdf5 file
  subroutine hdf_write_dataset_double_2(loc_id, dset_name, data)

    integer(HID_T), intent(in) :: loc_id        !< local id in file
    character(len=*), intent(in) :: dset_name   !< name of dataset
    real(dp), intent(in) :: data(:,:)           !< data to be written

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

  !>  \brief writes a 3d array to an hdf5 file
  subroutine hdf_write_dataset_double_3(loc_id, dset_name, data)

    integer(HID_T), intent(in) :: loc_id        !< local id in file
    character(len=*), intent(in) :: dset_name   !< name of dataset
    real(dp), intent(in) :: data(:,:,:)         !< data to be written

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

  !>  \brief writes a 4d array to an hdf5 file
  subroutine hdf_write_dataset_double_4(loc_id, dset_name, data)

    integer(HID_T), intent(in) :: loc_id        !< local id in file
    character(len=*), intent(in) :: dset_name   !< name of dataset
    real(dp), intent(in) :: data(:,:,:,:)       !< data to be written

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

  !>  \brief writes a 5d array to an hdf5 file
  subroutine hdf_write_dataset_double_5(loc_id, dset_name, data)

    integer(HID_T), intent(in) :: loc_id        !< local id in file
    character(len=*), intent(in) :: dset_name   !< name of dataset
    real(dp), intent(in) :: data(:,:,:,:,:)     !< data to be written

    integer :: rank
    integer(SIZE_T) :: dims(5)
    integer(HID_T) :: dset_id, dspace_id
    integer :: hdferror

    write(*,'(A)') "->hdf_write_dataset_double_4"

    ! set rank and dims
    rank = 5
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

  end subroutine hdf_write_dataset_double_5

  !>  \brief writes a 6d array to an hdf5 file
  subroutine hdf_write_dataset_double_6(loc_id, dset_name, data)

    integer(HID_T), intent(in) :: loc_id        !< local id in file
    character(len=*), intent(in) :: dset_name   !< name of dataset
    real(dp), intent(in) :: data(:,:,:,:,:,:)   !< data to be written

    integer :: rank
    integer(SIZE_T) :: dims(6)
    integer(HID_T) :: dset_id, dspace_id
    integer :: hdferror

    write(*,'(A)') "->hdf_write_dataset_double_4"

    ! set rank and dims
    rank = 6
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

  end subroutine hdf_write_dataset_double_6


!!----------------------------------------------------------------------------------------
  !!--------------------------------hdf_write_dataset_double--------------------------------
  !!----------------------------------------------------------------------------------------

  !>  \brief writes a scalar to an hdf5 file
  subroutine hdf_write_dataset_integer_0(loc_id, dset_name, data)

    integer(HID_T), intent(in) :: loc_id        !< local id in file
    character(len=*), intent(in) :: dset_name   !< name of dataset
    integer, intent(in) :: data                 !< data to be written

    integer(SIZE_T) :: dims(1)
    integer(HID_T) :: dset_id, dspace_id
    integer :: hdferror

    write(*,'(A)') "->hdf_write_dataset_integer_0"

    ! set rank and dims
    dims = (/ 0 /)

    ! create dataspace
    call h5screate_f(H5S_SCALAR_F, dspace_id, hdferror)
    !write(*,'(A20,I0)') "h5screate_simple: ", hdferror

    ! create dataset
    call h5dcreate_f(loc_id, dset_name, H5T_NATIVE_INTEGER, dspace_id, dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dcreate: ", hdferror

    ! write dataset
    call h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data, dims, hdferror)
    !write(*,'(A20,I0)') "h5dwrite: ", hdferror
    
    ! close all id's
    call h5dclose_f(dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dclose: ", hdferror
    call h5sclose_f(dspace_id, hdferror)
    !write(*,'(A20,I0)') "h5sclose: ", hdferror

  end subroutine hdf_write_dataset_integer_0

  !>  \brief writes a 1d array to an hdf5 file
  subroutine hdf_write_dataset_integer_1(loc_id, dset_name, data)

    integer(HID_T), intent(in) :: loc_id        !< local id in file
    character(len=*), intent(in) :: dset_name   !< name of dataset
    integer, intent(in) :: data(:)              !< data to be written

    integer :: rank
    integer(SIZE_T) :: dims(1)
    integer(HID_T) :: dset_id, dspace_id
    integer :: hdferror

    write(*,'(A)') "->hdf_write_dataset_integer_1"

    ! set rank and dims
    rank = 1
    dims = shape(data, KIND=HID_T)

    ! create dataspace
    call h5screate_simple_f(rank, dims, dspace_id, hdferror)
    !write(*,'(A20,I0)') "h5screate_simple: ", hdferror

    ! create dataset
    call h5dcreate_f(loc_id, dset_name, H5T_NATIVE_INTEGER, dspace_id, dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dcreate: ", hdferror

    ! write dataset
    call h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data, dims, hdferror)
    !write(*,'(A20,I0)') "h5dwrite: ", hdferror
    
    ! close all id's
    call h5dclose_f(dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dclose: ", hdferror
    call h5sclose_f(dspace_id, hdferror)
    !write(*,'(A20,I0)') "h5sclose: ", hdferror

  end subroutine hdf_write_dataset_integer_1

  !>  \brief writes a 2d array to an hdf5 file
  subroutine hdf_write_dataset_integer_2(loc_id, dset_name, data)

    integer(HID_T), intent(in) :: loc_id        !< local id in file
    character(len=*), intent(in) :: dset_name   !< name of dataset
    integer, intent(in) :: data(:,:)            !< data to be written

    integer :: rank
    integer(SIZE_T) :: dims(2)
    integer(HID_T) :: dset_id, dspace_id
    integer :: hdferror

    write(*,'(A)') "->hdf_write_dataset_integer_2"

    ! set rank and dims
    rank = 2
    dims = shape(data, KIND=HID_T)

    ! create dataspace
    call h5screate_simple_f(rank, dims, dspace_id, hdferror)
    !write(*,'(A20,I0)') "h5screate_simple: ", hdferror

    ! create dataset
    call h5dcreate_f(loc_id, dset_name, H5T_NATIVE_INTEGER, dspace_id, dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dcreate: ", hdferror

    ! write dataset
    call h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data, dims, hdferror)
    !write(*,'(A20,I0)') "h5dwrite: ", hdferror
    
    ! close all id's
    call h5dclose_f(dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dclose: ", hdferror
    call h5sclose_f(dspace_id, hdferror)
    !write(*,'(A20,I0)') "h5sclose: ", hdferror

  end subroutine hdf_write_dataset_integer_2

  !>  \brief writes a 3d array to an hdf5 file
  subroutine hdf_write_dataset_integer_3(loc_id, dset_name, data)

    integer(HID_T), intent(in) :: loc_id        !< local id in file
    character(len=*), intent(in) :: dset_name   !< name of dataset
    integer, intent(in) :: data(:,:,:)          !< data to be written

    integer :: rank
    integer(SIZE_T) :: dims(3)
    integer(HID_T) :: dset_id, dspace_id
    integer :: hdferror

    write(*,'(A)') "->hdf_write_dataset_integer_3"

    ! set rank and dims
    rank = 3
    dims = shape(data, KIND=HID_T)

    ! create dataspace
    call h5screate_simple_f(rank, dims, dspace_id, hdferror)
    !write(*,'(A20,I0)') "h5screate_simple: ", hdferror

    ! create dataset
    call h5dcreate_f(loc_id, dset_name, H5T_NATIVE_INTEGER, dspace_id, dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dcreate: ", hdferror

    ! write dataset
    call h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data, dims, hdferror)
    !write(*,'(A20,I0)') "h5dwrite: ", hdferror
    
    ! close all id's
    call h5dclose_f(dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dclose: ", hdferror
    call h5sclose_f(dspace_id, hdferror)
    !write(*,'(A20,I0)') "h5sclose: ", hdferror

  end subroutine hdf_write_dataset_integer_3

  !>  \brief writes a 4d array to an hdf5 file
  subroutine hdf_write_dataset_integer_4(loc_id, dset_name, data)

    integer(HID_T), intent(in) :: loc_id        !< local id in file
    character(len=*), intent(in) :: dset_name   !< name of dataset
    integer, intent(in) :: data(:,:,:,:)        !< data to be written

    integer :: rank
    integer(SIZE_T) :: dims(4)
    integer(HID_T) :: dset_id, dspace_id
    integer :: hdferror

    write(*,'(A)') "->hdf_write_dataset_integer_4"

    ! set rank and dims
    rank = 4
    dims = shape(data, KIND=HID_T)

    ! create dataspace
    call h5screate_simple_f(rank, dims, dspace_id, hdferror)
    !write(*,'(A20,I0)') "h5screate_simple: ", hdferror

    ! create dataset
    call h5dcreate_f(loc_id, dset_name, H5T_NATIVE_INTEGER, dspace_id, dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dcreate: ", hdferror

    ! write dataset
    call h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data, dims, hdferror)
    !write(*,'(A20,I0)') "h5dwrite: ", hdferror
    
    ! close all id's
    call h5dclose_f(dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dclose: ", hdferror
    call h5sclose_f(dspace_id, hdferror)
    !write(*,'(A20,I0)') "h5sclose: ", hdferror

  end subroutine hdf_write_dataset_integer_4

  !>  \brief writes a 5d array to an hdf5 file
  subroutine hdf_write_dataset_integer_5(loc_id, dset_name, data)

    integer(HID_T), intent(in) :: loc_id        !< local id in file
    character(len=*), intent(in) :: dset_name   !< name of dataset
    integer, intent(in) :: data(:,:,:,:,:)      !< data to be written

    integer :: rank
    integer(SIZE_T) :: dims(5)
    integer(HID_T) :: dset_id, dspace_id
    integer :: hdferror

    write(*,'(A)') "->hdf_write_dataset_integer_5"

    ! set rank and dims
    rank = 5
    dims = shape(data, KIND=HID_T)

    ! create dataspace
    call h5screate_simple_f(rank, dims, dspace_id, hdferror)
    !write(*,'(A20,I0)') "h5screate_simple: ", hdferror

    ! create dataset
    call h5dcreate_f(loc_id, dset_name, H5T_NATIVE_INTEGER, dspace_id, dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dcreate: ", hdferror

    ! write dataset
    call h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data, dims, hdferror)
    !write(*,'(A20,I0)') "h5dwrite: ", hdferror
    
    ! close all id's
    call h5dclose_f(dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dclose: ", hdferror
    call h5sclose_f(dspace_id, hdferror)
    !write(*,'(A20,I0)') "h5sclose: ", hdferror

  end subroutine hdf_write_dataset_integer_5

  !>  \brief writes a 6d array to an hdf5 file
  subroutine hdf_write_dataset_integer_6(loc_id, dset_name, data)

    integer(HID_T), intent(in) :: loc_id        !< local id in file
    character(len=*), intent(in) :: dset_name   !< name of dataset
    integer, intent(in) :: data(:,:,:,:,:,:)    !< data to be written

    integer :: rank
    integer(SIZE_T) :: dims(6)
    integer(HID_T) :: dset_id, dspace_id
    integer :: hdferror

    write(*,'(A)') "->hdf_write_dataset_integer_6"

    ! set rank and dims
    rank = 6
    dims = shape(data, KIND=HID_T)

    ! create dataspace
    call h5screate_simple_f(rank, dims, dspace_id, hdferror)
    !write(*,'(A20,I0)') "h5screate_simple: ", hdferror

    ! create dataset
    call h5dcreate_f(loc_id, dset_name, H5T_NATIVE_INTEGER, dspace_id, dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dcreate: ", hdferror

    ! write dataset
    call h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data, dims, hdferror)
    !write(*,'(A20,I0)') "h5dwrite: ", hdferror
    
    ! close all id's
    call h5dclose_f(dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dclose: ", hdferror
    call h5sclose_f(dspace_id, hdferror)
    !write(*,'(A20,I0)') "h5sclose: ", hdferror

  end subroutine hdf_write_dataset_integer_6

  
  !!---------------------------------------------------------------------------------------
  !!--------------------------------hdf_read_dataset_integer--------------------------------
  !!---------------------------------------------------------------------------------------


  !>  \brief reads a scalar from an hdf5 file
  subroutine hdf_read_dataset_integer_0(loc_id, dset_name, data)

    integer(HID_T), intent(in) :: loc_id        !< local id in file
    character(len=*), intent(in) :: dset_name   !< name of dataset
    integer, intent(out) :: data                !< data to be written


    integer(SIZE_T) :: dims(1)
    integer(HID_T) :: dset_id
    integer :: hdferror

    write(*,'(A)') "->hdf_read_dataset_double_0"

    ! set rank and dims
    dims = (/ 0 /)

    ! open dataset
    call h5dopen_f(loc_id, dset_name, dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dcreate: ", hdferror

    ! write dataset
    call h5dread_f(dset_id, H5T_NATIVE_INTEGER, data, dims, hdferror)
    !write(*,'(A20,I0)') "h5dwrite: ", hdferror
    
    ! close all id's
    call h5dclose_f(dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dclose: ", hdferror

  end subroutine hdf_read_dataset_integer_0

  !>  \brief reads a 1d array from an hdf5 file
  subroutine hdf_read_dataset_integer_1(loc_id, dset_name, data)

    integer(HID_T), intent(in) :: loc_id        !< local id in file
    character(len=*), intent(in) :: dset_name   !< name of dataset
    integer, intent(out) :: data(:)             !< data to be written

    integer :: rank
    integer(SIZE_T) :: dims(1)
    integer(HID_T) :: dset_id
    integer :: hdferror

    write(*,'(A)') "->hdf_read_dataset_integer_1"

    ! set rank and dims
    rank = 1
    dims = shape(data, KIND=HID_T)

    ! open dataset
    call h5dopen_f(loc_id, dset_name, dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dcreate: ", hdferror

    ! write dataset
    call h5dread_f(dset_id, H5T_NATIVE_INTEGER, data, dims, hdferror)
    !write(*,'(A20,I0)') "h5dwrite: ", hdferror
    
    ! close all id's
    call h5dclose_f(dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dclose: ", hdferror

  end subroutine hdf_read_dataset_integer_1

  !>  \brief reads a 2d array from an hdf5 file
  subroutine hdf_read_dataset_integer_2(loc_id, dset_name, data)

    integer(HID_T), intent(in) :: loc_id        !< local id in file
    character(len=*), intent(in) :: dset_name   !< name of dataset
    integer, intent(out) :: data(:,:)           !< data to be written

    integer :: rank
    integer(SIZE_T) :: dims(2)
    integer(HID_T) :: dset_id
    integer :: hdferror

    write(*,'(A)') "->hdf_read_dataset_integer_2"

    ! set rank and dims
    rank = 2
    dims = shape(data, KIND=HID_T)

    ! open dataset
    call h5dopen_f(loc_id, dset_name, dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dcreate: ", hdferror

    ! write dataset
    call h5dread_f(dset_id, H5T_NATIVE_INTEGER, data, dims, hdferror)
    !write(*,'(A20,I0)') "h5dwrite: ", hdferror
    
    ! close all id's
    call h5dclose_f(dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dclose: ", hdferror

  end subroutine hdf_read_dataset_integer_2

  !>  \brief reads a 3d array from an hdf5 file
  subroutine hdf_read_dataset_integer_3(loc_id, dset_name, data)

    integer(HID_T), intent(in) :: loc_id        !< local id in file
    character(len=*), intent(in) :: dset_name   !< name of dataset
    integer, intent(out) :: data(:,:,:)         !< data to be written

    integer :: rank
    integer(SIZE_T) :: dims(3)
    integer(HID_T) :: dset_id
    integer :: hdferror

    write(*,'(A)') "->hdf_read_dataset_integer_3"

    ! set rank and dims
    rank = 3
    dims = shape(data, KIND=HID_T)

    ! open dataset
    call h5dopen_f(loc_id, dset_name, dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dcreate: ", hdferror

    ! write dataset
    call h5dread_f(dset_id, H5T_NATIVE_INTEGER, data, dims, hdferror)
    !write(*,'(A20,I0)') "h5dwrite: ", hdferror
    
    ! close all id's
    call h5dclose_f(dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dclose: ", hdferror

  end subroutine hdf_read_dataset_integer_3

  !>  \brief reads a 4d array from an hdf5 file
  subroutine hdf_read_dataset_integer_4(loc_id, dset_name, data)

    integer(HID_T), intent(in) :: loc_id        !< local id in file
    character(len=*), intent(in) :: dset_name   !< name of dataset
    integer, intent(out) :: data(:,:,:,:)       !< data to be written

    integer :: rank
    integer(SIZE_T) :: dims(4)
    integer(HID_T) :: dset_id
    integer :: hdferror

    write(*,'(A)') "->hdf_read_dataset_integer_4"

    ! set rank and dims
    rank = 4
    dims = shape(data, KIND=HID_T)

    ! open dataset
    call h5dopen_f(loc_id, dset_name, dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dcreate: ", hdferror

    ! write dataset
    call h5dread_f(dset_id, H5T_NATIVE_INTEGER, data, dims, hdferror)
    !write(*,'(A20,I0)') "h5dwrite: ", hdferror
    
    ! close all id's
    call h5dclose_f(dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dclose: ", hdferror

  end subroutine hdf_read_dataset_integer_4

  !>  \brief reads a 5d array from an hdf5 file
  subroutine hdf_read_dataset_integer_5(loc_id, dset_name, data)

    integer(HID_T), intent(in) :: loc_id        !< local id in file
    character(len=*), intent(in) :: dset_name   !< name of dataset
    integer, intent(out) :: data(:,:,:,:,:)     !< data to be written

    integer :: rank
    integer(SIZE_T) :: dims(5)
    integer(HID_T) :: dset_id
    integer :: hdferror

    write(*,'(A)') "->hdf_read_dataset_integer_5"

    ! set rank and dims
    rank = 5
    dims = shape(data, KIND=HID_T)

    ! open dataset
    call h5dopen_f(loc_id, dset_name, dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dcreate: ", hdferror

    ! write dataset
    call h5dread_f(dset_id, H5T_NATIVE_INTEGER, data, dims, hdferror)
    !write(*,'(A20,I0)') "h5dwrite: ", hdferror
    
    ! close all id's
    call h5dclose_f(dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dclose: ", hdferror

  end subroutine hdf_read_dataset_integer_5

  !>  \brief reads a 6d array from an hdf5 file
  subroutine hdf_read_dataset_integer_6(loc_id, dset_name, data)

    integer(HID_T), intent(in) :: loc_id        !< local id in file
    character(len=*), intent(in) :: dset_name   !< name of dataset
    integer, intent(out) :: data(:,:,:,:,:,:)   !< data to be written

    integer :: rank
    integer(SIZE_T) :: dims(6)
    integer(HID_T) :: dset_id
    integer :: hdferror

    write(*,'(A)') "->hdf_read_dataset_integer_6"

    ! set rank and dims
    rank = 6
    dims = shape(data, KIND=HID_T)

    ! open dataset
    call h5dopen_f(loc_id, dset_name, dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dcreate: ", hdferror

    ! write dataset
    call h5dread_f(dset_id, H5T_NATIVE_INTEGER, data, dims, hdferror)
    !write(*,'(A20,I0)') "h5dwrite: ", hdferror
    
    ! close all id's
    call h5dclose_f(dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dclose: ", hdferror

  end subroutine hdf_read_dataset_integer_6


  
  !!---------------------------------------------------------------------------------------
  !!--------------------------------hdf_read_dataset_double--------------------------------
  !!---------------------------------------------------------------------------------------


  !>  \brief reads a scalar from an hdf5 file
  subroutine hdf_read_dataset_double_0(loc_id, dset_name, data)

    integer(HID_T), intent(in) :: loc_id        !< local id in file
    character(len=*), intent(in) :: dset_name   !< name of dataset
    real(dp), intent(out) :: data                 !< data to be written


    integer(SIZE_T) :: dims(1)
    integer(HID_T) :: dset_id
    integer :: hdferror

    write(*,'(A)') "->hdf_read_dataset_double_0"

    ! set rank and dims
    dims = (/ 0 /)

    ! open dataset
    call h5dopen_f(loc_id, dset_name, dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dcreate: ", hdferror

    ! write dataset
    call h5dread_f(dset_id, H5T_NATIVE_DOUBLE, data, dims, hdferror)
    !write(*,'(A20,I0)') "h5dwrite: ", hdferror
    
    ! close all id's
    call h5dclose_f(dset_id, hdferror)
    !write(*,'(A20,I0)') "h5dclose: ", hdferror

  end subroutine hdf_read_dataset_double_0

  !>  \brief reads a 1d array from an hdf5 file
  subroutine hdf_read_dataset_double_1(loc_id, dset_name, data)

    integer(HID_T), intent(in) :: loc_id        !< local id in file
    character(len=*), intent(in) :: dset_name   !< name of dataset
    real(dp), intent(out) :: data(:)            !< data to be written

    integer :: rank
    integer(SIZE_T) :: dims(1)
    integer(HID_T) :: dset_id
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

  !>  \brief reads a 2d array from an hdf5 file
  subroutine hdf_read_dataset_double_2(loc_id, dset_name, data)

    integer(HID_T), intent(in) :: loc_id        !< local id in file
    character(len=*), intent(in) :: dset_name   !< name of dataset
    real(dp), intent(out) :: data(:,:)          !< data to be written

    integer :: rank
    integer(SIZE_T) :: dims(2)
    integer(HID_T) :: dset_id
    integer :: hdferror

    write(*,'(A)') "->hdf_read_dataset_double_2"

    ! set rank and dims
    rank = 2
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

  end subroutine hdf_read_dataset_double_2

  !>  \brief reads a 3d array from an hdf5 file
  subroutine hdf_read_dataset_double_3(loc_id, dset_name, data)

    integer(HID_T), intent(in) :: loc_id        !< local id in file
    character(len=*), intent(in) :: dset_name   !< name of dataset
    real(dp), intent(out) :: data(:,:,:)        !< data to be written

    integer :: rank
    integer(SIZE_T) :: dims(3)
    integer(HID_T) :: dset_id
    integer :: hdferror

    write(*,'(A)') "->hdf_read_dataset_double_3"

    ! set rank and dims
    rank = 3
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

  end subroutine hdf_read_dataset_double_3

  !>  \brief reads a 4d array from an hdf5 file
  subroutine hdf_read_dataset_double_4(loc_id, dset_name, data)

    integer(HID_T), intent(in) :: loc_id        !< local id in file
    character(len=*), intent(in) :: dset_name   !< name of dataset
    real(dp), intent(out) :: data(:,:,:,:)      !< data to be written

    integer :: rank
    integer(SIZE_T) :: dims(4)
    integer(HID_T) :: dset_id
    integer :: hdferror

    write(*,'(A)') "->hdf_read_dataset_double_4"

    ! set rank and dims
    rank = 4
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

  end subroutine hdf_read_dataset_double_4

  !>  \brief reads a 5d array from an hdf5 file
  subroutine hdf_read_dataset_double_5(loc_id, dset_name, data)

    integer(HID_T), intent(in) :: loc_id        !< local id in file
    character(len=*), intent(in) :: dset_name   !< name of dataset
    real(dp), intent(out) :: data(:,:,:,:,:)    !< data to be written

    integer :: rank
    integer(SIZE_T) :: dims(5)
    integer(HID_T) :: dset_id
    integer :: hdferror

    write(*,'(A)') "->hdf_read_dataset_double_5"

    ! set rank and dims
    rank = 5
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

  end subroutine hdf_read_dataset_double_5

  !>  \brief reads a 6d array from an hdf5 file
  subroutine hdf_read_dataset_double_6(loc_id, dset_name, data)

    integer(HID_T), intent(in) :: loc_id        !< local id in file
    character(len=*), intent(in) :: dset_name   !< name of dataset
    real(dp), intent(out) :: data(:,:,:,:,:,:)  !< data to be written

    integer :: rank
    integer(SIZE_T) :: dims(6)
    integer(HID_T) :: dset_id
    integer :: hdferror

    write(*,'(A)') "->hdf_read_dataset_double_6"

    ! set rank and dims
    rank = 6
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

  end subroutine hdf_read_dataset_double_6

  
  !!---------------------------------------------------------------------------------------
  !!---------------------------------------hdf_write_attr*---------------------------------
  !!---------------------------------------------------------------------------------------

  !>  \brief writes a scalar attribute
  subroutine hdf_write_attr_double_0(loc_id, obj_name, attr_name, data)

    integer(HID_T), intent(in) :: loc_id        !< local id in file
    character(len=*), intent(in) :: obj_name    !< object name attribute will be attached to (if "" use loc_id)
    character(len=*), intent(in) :: attr_name   !< name of attribute
    real(dp), intent(in) :: data                !< data to write to attribute

    !integer :: rank
    integer(SIZE_T) :: dims(1)
    integer(HID_T) :: obj_id, aspace_id, attr_id
    integer :: hdferror

    write(*,'(A)') "->hdf_write_attr_double_0"

    ! open object
    if (obj_name == "") then
       obj_id = loc_id
    else
       call h5oopen_f(loc_id, obj_name, obj_id, hdferror)
    end if

    ! create dataspace
    dims = (/ 0 /)
    call h5screate_f(H5S_SCALAR_F, aspace_id, hdferror)
    !write(*,'(A20,I0)') "h5screate_simple: ", hdferror

    ! create attribute
    call h5acreate_f(obj_id, attr_name, H5T_NATIVE_DOUBLE, aspace_id, attr_id, hdferror)
    !write(*,'(A20,I0)') "h5dcreate: ", hdferror

    ! write dataset
    call h5awrite_f(attr_id, H5T_NATIVE_DOUBLE, data, dims, hdferror)
    !write(*,'(A20,I0)') "h5dwrite: ", hdferror
    
    ! close all id's
    call h5aclose_f(attr_id, hdferror)
    !write(*,'(A20,I0)') "h5dclose: ", hdferror
    call h5sclose_f(aspace_id, hdferror)
    !write(*,'(A20,I0)') "h5sclose: ", hdferror
    if (obj_name /= "") then
       call h5oclose_f(obj_id, hdferror)
    end if
    
  end subroutine hdf_write_attr_double_0

  !>  \brief writes 1d array attribute
  subroutine hdf_write_attr_double_1(loc_id, obj_name, attr_name, data)

    integer(HID_T), intent(in) :: loc_id        !< local id in file
    character(len=*), intent(in) :: obj_name    !< object name attribute will be attached to (if "" use loc_id)
    character(len=*), intent(in) :: attr_name   !< name of attribute
    real(dp), intent(in) :: data(:)             !< data to write to attribute

    integer :: rank
    integer(SIZE_T) :: dims(1)
    integer(HID_T) :: obj_id, aspace_id, attr_id
    integer :: hdferror

    write(*,'(A)') "->hdf_write_attr_double_1"

    ! open object
    if (obj_name == "") then
       obj_id = loc_id
    else
       call h5oopen_f(loc_id, obj_name, obj_id, hdferror)
    end if

    ! create dataspace
    rank = 1
    dims = shape(data, KIND=HID_T)
    call h5screate_simple_f(rank, dims, aspace_id, hdferror)
    !write(*,'(A20,I0)') "h5screate_simple: ", hdferror

    ! create attribute
    call h5acreate_f(obj_id, attr_name, H5T_NATIVE_DOUBLE, aspace_id, attr_id, hdferror)
    !write(*,'(A20,I0)') "h5dcreate: ", hdferror

    ! write dataset
    call h5awrite_f(attr_id, H5T_NATIVE_DOUBLE, data, dims, hdferror)
    !write(*,'(A20,I0)') "h5dwrite: ", hdferror
    
    ! close all id's
    call h5aclose_f(attr_id, hdferror)
    !write(*,'(A20,I0)') "h5dclose: ", hdferror
    call h5sclose_f(aspace_id, hdferror)
    !write(*,'(A20,I0)') "h5sclose: ", hdferror
    if (obj_name /= "") then
       call h5oclose_f(obj_id, hdferror)
    end if
    
  end subroutine hdf_write_attr_double_1

  !>  \brief writes a scalar attribute
  subroutine hdf_write_attr_integer_0(loc_id, obj_name, attr_name, data)

    integer(HID_T), intent(in) :: loc_id        !< local id in file
    character(len=*), intent(in) :: obj_name    !< object name attribute will be attached to (if "" use loc_id)
    character(len=*), intent(in) :: attr_name   !< name of attribute
    integer, intent(in) :: data                 !< data to write to attribute

    integer(SIZE_T) :: dims(1)
    integer(HID_T) :: obj_id, aspace_id, attr_id
    integer :: hdferror

    write(*,'(A)') "->hdf_write_attr_integer_0"

    ! open object
    if (obj_name == "") then
       obj_id = loc_id
    else
       call h5oopen_f(loc_id, obj_name, obj_id, hdferror)
    end if

    ! create dataspace
    dims = (/ 0 /)
    call h5screate_f(H5S_SCALAR_F, aspace_id, hdferror)
    !write(*,'(A20,I0)') "h5screate_simple: ", hdferror

    ! create attribute
    call h5acreate_f(obj_id, attr_name, H5T_NATIVE_INTEGER, aspace_id, attr_id, hdferror)
    !write(*,'(A20,I0)') "h5dcreate: ", hdferror

    ! write dataset
    call h5awrite_f(attr_id, H5T_NATIVE_INTEGER, data, dims, hdferror)
    !write(*,'(A20,I0)') "h5dwrite: ", hdferror
    
    ! close all id's
    call h5aclose_f(attr_id, hdferror)
    !write(*,'(A20,I0)') "h5dclose: ", hdferror
    call h5sclose_f(aspace_id, hdferror)
    !write(*,'(A20,I0)') "h5sclose: ", hdferror
    if (obj_name /= "") then
       call h5oclose_f(obj_id, hdferror)
    end if
    
  end subroutine hdf_write_attr_integer_0

  !>  \brief writes 1d array attribute
  subroutine hdf_write_attr_integer_1(loc_id, obj_name, attr_name, data)

    integer(HID_T), intent(in) :: loc_id        !< local id in file
    character(len=*), intent(in) :: obj_name    !< object name attribute will be attached to (if "" use loc_id)
    character(len=*), intent(in) :: attr_name   !< name of attribute
    integer, intent(in) :: data(:)              !< data to write to attribute

    integer :: rank
    integer(SIZE_T) :: dims(1)
    integer(HID_T) :: obj_id, aspace_id, attr_id
    integer :: hdferror

    write(*,'(A)') "->hdf_write_attr_integer_1"

    ! open object
    if (obj_name == "") then
       obj_id = loc_id
    else
       call h5oopen_f(loc_id, obj_name, obj_id, hdferror)
    end if

    ! create dataspace
    rank = 1
    dims = shape(data, KIND=HID_T)
    call h5screate_simple_f(rank, dims, aspace_id, hdferror)
    !write(*,'(A20,I0)') "h5screate_simple: ", hdferror

    ! create attribute
    call h5acreate_f(obj_id, attr_name, H5T_NATIVE_INTEGER, aspace_id, attr_id, hdferror)
    !write(*,'(A20,I0)') "h5dcreate: ", hdferror

    ! write dataset
    call h5awrite_f(attr_id, H5T_NATIVE_INTEGER, data, dims, hdferror)
    !write(*,'(A20,I0)') "h5dwrite: ", hdferror
    
    ! close all id's
    call h5aclose_f(attr_id, hdferror)
    !write(*,'(A20,I0)') "h5dclose: ", hdferror
    call h5sclose_f(aspace_id, hdferror)
    !write(*,'(A20,I0)') "h5sclose: ", hdferror
    if (obj_name /= "") then
       call h5oclose_f(obj_id, hdferror)
    end if
    
  end subroutine hdf_write_attr_integer_1

!!$  !>  \brief writes a scalar attribute
!!$  subroutine hdf_write_attr_integer_0()
!!$  end subroutine hdf_write_attr_integer_0

!!$  !>  \brief writes a scalar attribute
!!$  subroutine hdf_write_attr_integer_1()
!!$  end subroutine hdf_write_attr_integer_1

  !>  \brief writes a string attribute
  subroutine hdf_write_attr_string(loc_id, obj_name, attr_name, data)
  
    integer(HID_T), intent(in) :: loc_id        !< local id in file
    character(len=*), intent(in) :: obj_name    !< object name attribute will be attached to (if "" use loc_id)
    character(len=*), intent(in) :: attr_name   !< name of attribute
    character(len=*), intent(in) :: data        !< data to write to attribute

    !integer :: rank, str_len
    integer(SIZE_T) :: dims(1)
    integer(HID_T) :: obj_id, type_id, aspace_id, attr_id
    integer :: hdferror

    write(*,'(A)') "->hdf_write_attr_string"

    ! open object
    if (obj_name == "") then
       obj_id = loc_id
    else
       call h5oopen_f(loc_id, obj_name, obj_id, hdferror)
    end if

    ! create type_id and aspace_id
    dims(1) = len(data, KIND=HID_T)
    call h5tcopy_f (H5T_NATIVE_CHARACTER, type_id, hdferror)
    !write(*,*) 'h5tcopy_f returns', type_id 
    call h5tset_size_f (type_id, dims(1), hdferror)
    !write(*,*) 'h5tset_size_f returns', hdferror
    call h5screate_f (H5S_SCALAR_F, aspace_id, hdferror)

    ! create attribute
    call h5acreate_f(obj_id, attr_name, type_id, aspace_id, attr_id, hdferror)
    !write(*,'(A20,I0)') "h5dcreate: ", hdferror

    ! write dataset
    call h5awrite_f(attr_id, type_id, data, dims, hdferror)
    !write(*,'(A20,I0)') "h5dwrite: ", hdferror
    
    ! close all id's
    call h5tclose_f(type_id, hdferror)
    call h5aclose_f(attr_id, hdferror)
    !write(*,'(A20,I0)') "h5dclose: ", hdferror
    call h5sclose_f(aspace_id, hdferror)
    !write(*,'(A20,I0)') "h5sclose: ", hdferror
    if (obj_name /= "") then
       call h5oclose_f(obj_id, hdferror)
    end if
    
  end subroutine hdf_write_attr_string


  !!---------------------------------------------------------------------------------------
  !!-------------------------------------hdf_read_attr-------------------------------------
  !!---------------------------------------------------------------------------------------

  !>  \brief writes a scalar attribute
  subroutine hdf_read_attr_double_0(loc_id, obj_name, attr_name, data)

    integer(HID_T), intent(in) :: loc_id        !< local id in file
    character(len=*), intent(in) :: obj_name    !< object name attribute will be attached to (if "" use loc_id)
    character(len=*), intent(in) :: attr_name   !< name of attribute
    real(dp), intent(out) :: data                !< data to write to attribute

    integer(SIZE_T) :: dims(1)
    integer(HID_T) :: obj_id, attr_id
    integer :: hdferror

    write(*,'(A)') "->hdf_read_attr_double_0"

    ! open object
    if (obj_name == "") then
       obj_id = loc_id
    else
       call h5oopen_f(loc_id, obj_name, obj_id, hdferror)
    end if

    ! create attribute
    call h5aopen_f(obj_id, attr_name, attr_id, hdferror)
    !write(*,'(A20,I0)') "h5dcreate: ", hdferror

    ! write dataset
    call h5aread_f(attr_id, H5T_NATIVE_DOUBLE, data, dims, hdferror)
    !write(*,'(A20,I0)') "h5dwrite: ", hdferror
    
    ! close all id's
    call h5aclose_f(attr_id, hdferror)
    !write(*,'(A20,I0)') "h5dclose: ", hdferror
    if (obj_name /= "") then
       call h5oclose_f(obj_id, hdferror)
    end if
    
  end subroutine hdf_read_attr_double_0

  !>  \brief reads 1d array attribute
  subroutine hdf_read_attr_double_1(loc_id, obj_name, attr_name, data)

    integer(HID_T), intent(in) :: loc_id        !< local id in file
    character(len=*), intent(in) :: obj_name    !< object name attribute will be attached to (if "" use loc_id)
    character(len=*), intent(in) :: attr_name   !< name of attribute
    real(dp), intent(out) :: data(:)             !< data to write to attribute

    integer :: rank
    integer(SIZE_T) :: dims(1)
    integer(HID_T) :: obj_id, attr_id
    integer :: hdferror

    write(*,'(A)') "->hdf_read_attr_double_1"

    ! open object
    if (obj_name == "") then
       obj_id = loc_id
    else
       call h5oopen_f(loc_id, obj_name, obj_id, hdferror)
    end if

    ! create dataspace
    rank = 1
    dims = shape(data, KIND=HID_T)

    ! create attribute
    call h5aopen_f(obj_id, attr_name, attr_id, hdferror)
    !write(*,'(A20,I0)') "h5dcreate: ", hdferror

    ! write dataset
    call h5aread_f(attr_id, H5T_NATIVE_DOUBLE, data, dims, hdferror)
    !write(*,'(A20,I0)') "h5dwrite: ", hdferror
    
    ! close all id's
    call h5aclose_f(attr_id, hdferror)
    !write(*,'(A20,I0)') "h5dclose: ", hdferror
    if (obj_name /= "") then
       call h5oclose_f(obj_id, hdferror)
    end if
    
  end subroutine hdf_read_attr_double_1

!!$  !>  \brief writes a scalar attribute
!!$  subroutine hdf_write_attr_integer_0()
!!$  end subroutine hdf_write_attr_integer_0

!!$  !>  \brief writes a scalar attribute
!!$  subroutine hdf_write_attr_integer_1()
!!$  end subroutine hdf_write_attr_integer_1

  !>  \brief writes a string attribute
  subroutine hdf_read_attr_string(loc_id, obj_name, attr_name, data)

    integer(HID_T), intent(in) :: loc_id        !< local id in file
    character(len=*), intent(in) :: obj_name    !< object name attribute will be attached to (if "" use loc_id)
    character(len=*), intent(in) :: attr_name   !< name of attribute
    character(len=*), intent(out) :: data        !< data to write to attribute

    !integer :: rank, str_len
    integer(SIZE_T) :: dims(1)
    integer(HID_T) :: obj_id, type_id, attr_id
    integer :: hdferror

    write(*,'(A)') "->hdf_write_attr_string"

    ! open object
    if (obj_name == "") then
       obj_id = loc_id
    else
       call h5oopen_f(loc_id, obj_name, obj_id, hdferror)
    end if

    ! create type_id
    dims(1) = len(data, KIND=HID_T)
    call h5tcopy_f (H5T_NATIVE_CHARACTER, type_id, hdferror)
    !write(*,*) 'h5tcopy_f returns', type_id 
    call h5tset_size_f (type_id, dims(1), hdferror)
    
    ! create attribute
    call h5aopen_f(obj_id, attr_name, attr_id, hdferror)
    !write(*,'(A20,I0)') "h5dcreate: ", hdferror

    ! write dataset
    call h5aread_f(attr_id, type_id, data, dims, hdferror)
    !write(*,'(A20,I0)') "h5dwrite: ", hdferror

    ! close all id's
    call h5aclose_f(attr_id, hdferror)
    !write(*,'(A20,I0)') "h5dclose: ", hdferror
    if (obj_name /= "") then
       call h5oclose_f(obj_id, hdferror)
    end if

  end subroutine hdf_read_attr_string

end module HDF5_utils
