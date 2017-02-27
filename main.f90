program hdf5_test

  !use, intrinsic :: iso_fortran_env, dp=>real64
  use kinds
  implicit none
  
  integer :: ii, jj, kk, ll
  integer :: data0,  data2(4,6), data4(4,6,8,10)
  real(dp) :: data1(8), data3(4,6,8)

  ! fill in data
  data0 = 42
  
  do ii = 1, 8
     data1(ii) = 10 + ii
  end do
  
  do ii = 1, 4
     do jj = 1, 6
        data2(ii,jj) = (ii-1)*6 + jj
     end do
  end do

  do ii = 1, 4
     do jj = 1, 6
        do kk = 1, 8
           data3(ii,jj,kk) = ((ii-1)*6 + jj -1)*8 + kk
        end do
     end do
  end do

  do ii = 1, 4
     do jj = 1, 6
        do kk = 1, 8
           do ll = 1, 10
              data4(ii,jj,kk,ll) = (((ii-1)*6 + jj -1)*8 + kk - 1)*10 + ll
           end do
        end do
     end do
  end do

  !call hdf5_types()
  !call test_low_level()

  call test_high_level()

contains

 
  !>  \brief write to an hdf5 file using the low level subroutines
  subroutine test_high_level()

    use hdf5_utils

    integer(HID_T) :: file_id, group_id

    character(len=8) :: date
    character(len=10) :: time
    character(len=32) :: ostring

    integer :: rank, dims(6)
    integer, allocatable :: test4(:,:,:,:)
    
    write(*,'(A)') "test_high_level"

    !
    ! the basics
    !

    !call hdf_set_print_messages(.true.)
    
    ! open file
    call hdf_open_file(file_id, "test_hl.h5", STATUS='NEW')

    ! write out some datasets
    call hdf_write_dataset(file_id, "data0", data0)
    call hdf_write_dataset(file_id, "data1", data1)
    call hdf_write_dataset(file_id, "data2", data2)
    call hdf_write_dataset(file_id, "data3", data3)
    call hdf_write_dataset(file_id, "data4", data4)

    ! close file
    call hdf_close_file(file_id)

    
    !    
    ! test reading
    !
    
    ! open file
    call hdf_open_file(file_id, "test_hl.h5", STATUS='OLD', ACTION='READ')

    ! read in some datasets 
    call hdf_read_dataset(file_id, "data0", data0)
    call hdf_read_dataset(file_id, "data1", data1)
    call hdf_read_dataset(file_id, "data2", data2)
    call hdf_read_dataset(file_id, "data3", data3)
    call hdf_read_dataset(file_id, "data4", data4)
    
    ! close file
    call hdf_close_file(file_id)


    !
    ! use attribute
    !

    ! open file
    call hdf_open_file(file_id, "test_hl.h5", STATUS='OLD', ACTION='READWRITE')

    ! write attribute to a dataset
    call hdf_write_attribute(file_id, "data1", "rank", 7)
    call hdf_write_attribute(file_id, "data4", "luckynumber", 1.618_dp)
    
    ! write attribute to the file (and get version from Makefile)
    call date_and_time(DATE=date, TIME=time)
    call hdf_write_attribute(file_id, "", "date/time", date // ": " // time)
#if defined(VERSION)
    call hdf_write_attribute(file_id, "", "version", VERSION)
#endif

    ! close file
    call hdf_close_file(file_id)
    
    
    !
    ! use groups
    !

    ! open file
    call hdf_open_file(file_id, "test_groups.h5", STATUS='NEW')

    ! absolute access
    call hdf_create_group(file_id, "group1")
    call hdf_write_dataset(file_id, "group1/data1", data1)
    call hdf_write_attribute(file_id, "group1", "tag", "this is group 1")
    call hdf_write_attribute(file_id, "group1/data1", "tag", "this is data1 in group 1")

    ! relative access
    call hdf_create_group(file_id, "group2")
    call hdf_open_group(file_id, "group2", group_id)
    call hdf_write_dataset(group_id, "data2", data2)
    call hdf_write_attribute(group_id, "", "tag", "this is group 2")
    call hdf_write_attribute(group_id, "data2", "tag", "this is data2 in group 2")
    call hdf_close_group(group_id)

    ! close file
    call hdf_close_file(file_id)


    
    !
    ! test reading full
    !
    
    call hdf_open_file(file_id, "test_hl.h5", STATUS='OLD', ACTION='READ')
    
    call hdf_read_dataset(file_id, "data0", data0)
    call hdf_read_dataset(file_id, "data1", data1)
    call hdf_read_dataset(file_id, "data2", data2)
    call hdf_read_dataset(file_id, "data3", data3)
    call hdf_read_dataset(file_id, "data4", data4)

    call hdf_read_attribute(file_id, "data1", "rank", data0)
    write(*,*) "rank: ", data0
    call hdf_read_attribute(file_id, "", "version", ostring)
    write(*,*) "version: ", ostring
    call hdf_read_attribute(file_id, "", "date/time", ostring)
    write(*,*) "date/time: ", ostring

    ! close file
    call hdf_close_file(file_id)


    !
    ! reading an unkown size
    !

    ! open file
    call hdf_open_file(file_id, "test_hl.h5", STATUS='OLD', ACTION='READ')

    ! get rank
    call hdf_get_rank(file_id, "data4", rank)
    write(*,*) "rank(data4) = ", rank

    ! get dimensions 
    call hdf_get_dims(file_id, "data4", dims)
    write(*,*) "dims(data4) = ", dims(1:rank)

    ! allocate and read
    allocate ( test4(dims(1), dims(2), dims(3), dims(4)) )
    call hdf_read_dataset(file_id, "data4", data4)

    ! close file
    call hdf_close_file(file_id)

    
  end subroutine test_high_level


  !>  \brief write out some H5T types for reference
  subroutine hdf5_types()
    use hdf5

    integer :: hdferror

    call h5open_f(hdferror)

    write(*,*) H5T_NATIVE_DOUBLE
    write(*,*) H5T_IEEE_F64BE
    write(*,*) H5T_IEEE_F64LE

    write(*,*) H5T_NATIVE_REAL
    write(*,*) H5T_IEEE_F32BE
    write(*,*) H5T_IEEE_F32LE 
    
    call h5close_f(hdferror)

  end subroutine hdf5_types


  !>  \brief write to an hdf5 file using the low level subroutines
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

end program hdf5_test
