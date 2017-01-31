program hdf5_test

  use precision, only: dp
  implicit none
  
  integer :: ii, jj
  real(dp) :: data1(8), data2(4,6)

  ! fill in data
  do ii = 1, 8
     data1(ii) = 10 + ii
  end do
  do ii = 1, 4
     do jj = 1, 6
        data2(ii,jj) = (ii-1)*6 + jj
     end do
  end do

  call test_low_level()

  call test_high_level()

contains


  subroutine test_low_level()

    use hdf5

    character(len=16) :: filename = "test_ll.h5"

    integer(HID_T) :: file_id, dset_id, dspace_id
    integer :: rank
    integer(SIZE_T) :: dims(2)
    integer :: hdferror

    write(*,'(A)') "test_low_level"
    
    ! open hdf5 interface
    call h5open_f(hdferror)
    write(*,'(A20,I0)') "h5open: ", hdferror

    ! open/create hdf5 file
    call h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, hdferror, H5P_DEFAULT_F, H5P_DEFAULT_F)
    !call h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, hdferror)
    write(*,'(A20,I0)') "h5fcreate: ", hdferror

    ! create dataspace
    rank = 2
    dims = (/ 4, 6 /)
    call h5screate_simple_f(rank, dims, dspace_id, hdferror)
    write(*,'(A20,I0)') "h5screate_simple: ", hdferror

    ! create dataset
    call h5dcreate_f(file_id, "data2", H5T_NATIVE_DOUBLE, dspace_id, dset_id, hdferror)
    write(*,'(A20,I0)') "h5dcreate: ", hdferror

    ! write dataset
    call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, data2, dims, hdferror)
    write(*,'(A20,I0)') "h5dwrite: ", hdferror

    ! close all id's
    call h5dclose_f(dset_id, hdferror)
    write(*,'(A20,I0)') "h5dclose: ", hdferror

    call h5sclose_f(dspace_id, hdferror)
    write(*,'(A20,I0)') "h5sclose: ", hdferror

    call h5fclose_f(file_id, hdferror)
    write(*,'(A20,I0)') "h5fclose: ", hdferror

    call h5close_f(hdferror)
    write(*,'(A20,I0)') "h5close: ", hdferror


  end subroutine test_low_level

  
  subroutine test_high_level()

    use hdf5_utils

    character(len=16) :: filename = "test_hl.h5"

    integer(HID_T) :: file_id

    write(*,'(A)') "test_high_level"

    call hdf_open(file_id, filename)

    call hdf_write_dataset_double2(file_id, data2, "data2")
    
    call hdf_close(file_id)

  end subroutine test_high_level

  
end program hdf5_test
