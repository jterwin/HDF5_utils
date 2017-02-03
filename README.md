# HDF5_utils

The purpose of this library is to provide a high level interface into
[HDF5](https://support.hdfgroup.org/HDF5/), read a user friendly interface into HDF5 but with limit scope.



## The Basics

Here is a simple examle of writing to a new HDF5 file

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
The rank, dimension, and datatypes of the datasets in the HDF5 file
correspond to the arrays passed to it.


Here is a simple examle of reading from an HDF5 file

```fortran
	use hdf5_utils
	
	integer(HID_T) :: file_id
	
	integer :: data0 = 12
	integer :: data2(2,4)
	real(dp) :: data3(4,6,8)
	
	! open file
	call hdf_open_file(file_id, "outfile.h5", STATUS='OLD', ACTION='READ')
	
	! write out some datasets
	call hdf_read_dataset(file_id, "data0", data0)
	call hdf_read_dataset(file_id, "data2", data2)
	call hdf_read_dataset(file_id, "data3", data3)
	
	! close file
	call hdf_close_file(file_id)

```
The rank, dimension, and datatypes array should match that of the
datasets in the HDF5 file, otherwise the HDF5 library will bring up
and error.

## Using Attributes

Attribute are just like data sets, but they can be attached to
datasets, group, or the file root. The suggested use is for small
items like the code version or run parameters.

```fortran
    use hdf5_utils
    
    call hdf_open_file(file_id, "outfile.h5", STATUS='NEW')
    
    ! adding attribute to root of file
    call hdf_write_attribute(file_id, "", "luckynumber", 7)
    
    ! adding attribute to dataset
    call hdf_write_attribute(file_id, "data1", "rank", 1.618_dp)
    
    ! close file
    call hdf_close_file(file_id)
    
```
The rank, dimension, and datatypes of the attribute in the HDF5 file
correspond to the arrays passed to it. Further, there is the
`hdf_read_attribute` subroutine.


## Using Groups

Groups are a nice way to organize datasets and attributes within the
HDF5 file. They work like directories/folder on a computer. When using
group, you can access their contents using absolute or relative paths.
```fortran
    use hdf5_utils
    
    integer(HID_T) :: file_id, group_id
    
    ! open file
    call hdf_open_file(file_id, "outfile.h5", STATUS='NEW')
    
    ! using absolute access
    call hdf_create_group(file_id, "group1")
    call hdf_write_dataset(file_id, "group1/gdata1", data1)
    call hdf_write_attribute(file_id, "group1", "tag", "this is group 1")
    
    ! or using relative access
    call hdf_create_group(file_id, "group2")
    call hdf_open_group(file_id, "group2", group_id)
    call hdf_write_dataset(group_id, "gdata2", data2)
    call hdf_write_attribute(group_id, "", "tag", "this is group 2")
    call hdf_close_group(group_id)
    
    ! close file
    call hdf_close_file(file_id)
    
```
Notice in the relative path case, we can pass the `group_id` to
read/write subroutines instead of the file_id. This can be used for
cinvenient access to nested objects.

