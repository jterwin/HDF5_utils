# HDF5_utils

The purpose of this library is to provide a high level interface into HDF5, read a user friendly interface into HDF5 but with limit scope.


## basics The Basics

Here is a summary of the Basics

```fortran
	use hdf5_utils
	
	integer(HID_T) :: file_id
	
	integer :: data0 = 12
	integer :: data2(2,4)
	real(dp) :: data3(4,6,8)
	
	! open file
	call hdf_open_file(file_id, "outfile.h5", STATUS='NEW')
	
	! write out some datasets
	call hdf_write_dataset(file_id, "data0", data0)
	call hdf_write_dataset(file_id, "data2", data2)
	call hdf_write_dataset(file_id, "data3", data3)
	
	! close file
	call hdf_close_file(file_id)

```

