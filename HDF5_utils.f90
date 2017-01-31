!
!
!
module precision
  integer, parameter :: dp = kind(1.0d0)
end module precision


!
!
!
module HDF5_utils
  
  use precision, only: dp
  use hdf5
  implicit none

contains

  !
  !
  !
  subroutine hdf_open(file_id, filename, STATUS, ACTION)

    integer(HID_T), intent(out) :: file_id
    character(len=*), intent(in) :: filename
    character(len=*), optional, intent(in) :: STATUS
    character(len=*), optional, intent(in) :: ACTION

    integer :: hdferror

    write(*,'(A)') "->hdf_open"
    
    ! open hdf5 interface
    call h5open_f(hdferror)
    write(*,'(A20,I0)') "h5open: ", hdferror

    !
    if (present(STATUS)) write(*,'(A,A)') "STATUS = ", STATUS
    if (present(STATUS)) write(*,'(A,A)') "ACTION = ", ACTION

    ! open/create hdf5 file
    call h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, hdferror, H5P_DEFAULT_F, H5P_DEFAULT_F)
    !call h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, hdferror)
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


  !
  !
  !
  subroutine hdf_write_dataset_double2(loc_id, data, dset_name)

    integer(HID_T), intent(in) :: loc_id
    real(dp), intent(in) :: data(:,:)
    character(len=*), intent(in) :: dset_name

    integer :: rank
    integer(SIZE_T) :: dims(2)
    integer(HID_T) :: dset_id, dspace_id
    integer :: hdferror

    write(*,'(A)') "->hdf_write_dataset_double2"

    ! set rank and dims
    rank = 2
    dims = shape(data, KIND=HID_T)

    ! create dataspace
    call h5screate_simple_f(rank, dims, dspace_id, hdferror)
    write(*,'(A20,I0)') "h5screate_simple: ", hdferror

    ! create dataset
    call h5dcreate_f(loc_id, dset_name, H5T_NATIVE_DOUBLE, dspace_id, dset_id, hdferror)
    write(*,'(A20,I0)') "h5dcreate: ", hdferror

    ! write dataset
    call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, data, dims, hdferror)
    write(*,'(A20,I0)') "h5dwrite: ", hdferror
    
    ! close all id's
    call h5dclose_f(dset_id, hdferror)
    write(*,'(A20,I0)') "h5dclose: ", hdferror
    call h5sclose_f(dspace_id, hdferror)
    write(*,'(A20,I0)') "h5sclose: ", hdferror

  end subroutine hdf_write_dataset_double2
    

end module HDF5_utils
