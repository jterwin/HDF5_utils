# HDF5_utils

This library is to provide a high level interface into [HDF5](https://portal.hdfgroup.org/display/support).
This is, of course, based off HDF5's own High-Level module ([HDLT](https://support.hdfgroup.org/HDF5/doc/HL/RM_H5LT.html)) but customized to my own needs.

The aim of this library is to:
 - Abstract most of the HDF5 tyes.
 - Read/write datasets of multiple types and ranks.
   - Assumed reading/writing full arrays.
   - Remove need to pass dimensions (allocation and consistency of dimensions is left to the user). 
 - Write/read attributes to file/group/datasets.
 - Provide the ability to create groups, and access datasets by either absolute or relative path.
 - Some auxiliary functions:
   - Check if dataset/object exists.
   - Get rank and size of dataset, either to check dimensions or to allocate array before reading.

## Module summary

This is just a short summary to provide a basic understandig of the module.
For a more detailed explaination, look at the code comments.
If you have doxygen installed, `make docs` will produce an html output in `docs/html/index.html`.

subroutine   | inputs   | description 
:---- |  :---- |  :----
`hdf_open_file` | `file_id`, `filename`, `STATUS`, `ACTION` | Opens file and return identifier
`hdf_close_file` | `file_id` | Closes a hdf5 file
`hdf_write_dataset` | `loc_id`, `dset_name`, `data` | write out array (or scalar)
`hdf_read_dataset` | `loc_id`, `dset_name`, `data` | read in array (or scalar)
`hdf_create_dataset` | `loc_id`, `dset_name`, `dset_dims`, `dset_type` | creates an empty dataset
`hdf_write_vector_to_dataset` | `loc_id`, `dset_name`, `offset`, `vector` | write a vector to leading edge of dataset
`hdf_read_vector_from_dataset` | `loc_id`, `dset_name`, `offset`, `vector` | read a 1d array from from leading edge of dataset
`hdf_write_attribute` | `loc_id`, `obj_name`, `attr_name`, `data` | write out attribute array (or scalar)
`hdf_read_attribute` | `loc_id`, `obj_name`, `attr_name`, `data` | read in attribute array (or scalar)
`hdf_create_group` | `loc_id`, `group_name` | Create a new group
`hdf_open_group` | `loc_id`, `group_name`, `group_id` | Opens a group and returns the identifier
`hdf_close_group` | `group_id` | Close a group by identifier
`hdf_get_rank` | `loc_id`, `dset_name`, `rank` | Get the rank of a dataset
`hdf_get_dims` | `loc_id`, `dset_name`, `dims` | get the dimensions of a dataset
`hdf_exists` | `loc_id`, `obj_name` | Check if location exists
`hdf_set_print_messages` | `val_print_messages` | Sets the value of `hdf_print_messages`

## Examples


### The Basics

Here is a simple example of writing to a new HDF5 file:

```fortran
    use hdf5_utils
    
    integer(HID_T) :: file_id
    
    integer :: ii, jj, kk, ll
    integer :: data0, data1(8), data2(4,6), data3(4,6,8), data4(4,6,8,10)
    real(sp) :: data2_sp(4,6)
    real(dp) :: data2_dp(4,6)
    
    !
    call hdf_set_print_messages(.true.)
    
    ! open file
    call hdf_open_file(file_id, "test_hl.h5", STATUS='NEW')
    
    ! write out some datasets
    call hdf_write_dataset(file_id, "data0", data0)
    call hdf_write_dataset(file_id, "data1", data1)
    call hdf_write_dataset(file_id, "data2", data2)
    call hdf_write_dataset(file_id, "data3", data3)
    call hdf_write_dataset(file_id, "data4", data4)

    call hdf_write_dataset(file_id, "data2_sp", data2_sp)
    call hdf_write_dataset(file_id, "data2_dp", data2_dp)

    ! close file
    call hdf_close_file(file_id)

```

The rank, dimension, and datatypes of the datasets in the HDF5 file correspond to the arrays passed to it.

Here is a simple examle of reading from an HDF5 file:

```fortran
    use hdf5_utils
    
    integer(HID_T) :: file_id
    
    integer :: data0, data1(8), data2(4,6), data3(4,6,8), data4(4,6,8,10)
    real(sp) :: data2_sp(4,6)
    real(dp) :: data2_dp(4,6)
    
    ! open file
    call hdf_open_file(file_id, "test_hl.h5", STATUS='OLD', ACTION='READ')

    ! read in some datasets 
    call hdf_read_dataset(file_id, "data0", data0)
    call hdf_read_dataset(file_id, "data1", data1)
    call hdf_read_dataset(file_id, "data2", data2)
    call hdf_read_dataset(file_id, "data3", data3)
    call hdf_read_dataset(file_id, "data4", data4)

    call hdf_read_dataset(file_id, "data2_sp", data2_sp)
    call hdf_read_dataset(file_id, "data2_dp", data2_dp)
    
    ! close file
    call hdf_close_file(file_id)

```

The rank, dimension, and datatypes array should match that of the datasets in the HDF5 file, otherwise the HDF5 library will bring up and error.

Note that HDF5 will convert the datatype on reading. So based on the above example
```fortran
    use hdf5_utils
    
    integer(HID_T) :: file_id
    integer :: ii
    
    integer :: data2(4,6)
    real(sp) :: data2_sp(4,6)
    real(dp) :: data2_dp(4,6)
    
    ! open file
    call hdf_open_file(file_id, "test_hl.h5", STATUS='OLD', ACTION='READ')

    ! read in some datasets 
    call hdf_read_dataset(file_id, "data2_dp", data2)
    call hdf_read_dataset(file_id, "data2_dp", data2_sp)
    call hdf_read_dataset(file_id, "data2_dp", data2_dp)

    do ii = 1,4
      write(*,'(10(I5,3x))') data2(ii,:)
    enddo
    do ii = 1,4
      write(*,'(10(F7.1,1x))') data2_sp(ii,:)
    enddo
    do ii = 1,4
      write(*,'(10F8.2)') data2_dp(ii,:)
    enddo
    
    ! close file
    call hdf_close_file(file_id)

```

This example will read in from a double precision dataset into an integer, real, and double precision array. 
Note that the integer array will be the floor of the double/real array.

### Using Attributes

Attribute are just like data sets, but they can be attached to datasets, group, or the file root.
The suggested use is for small items like the code version or run parameters:

```fortran
    use hdf5_utils
    
    ! open file
    call hdf_open_file(file_id, "test_hl.h5", STATUS='OLD', ACTION='READWRITE')

    ! write attribute to a dataset
    call hdf_write_attribute(file_id, "data1", "rank", 7)
    call hdf_write_attribute(file_id, "data4", "luckynumber", 1.618_dp)
    
    ! write attribute to the file (and get version from Makefile)
    call date_and_time(DATE=date, TIME=time)
    call hdf_write_attribute(file_id, "", "date/time", date // ": " // time)

    ! close file
    call hdf_close_file(file_id)
    
```

The rank, dimension, and datatypes of the attribute in the HDF5 file correspond to the arrays passed to it.
Further, there is the `hdf5_utils::hdf_read_attribute` subroutine.


### Using Groups

Groups are a nice way to organize datasets and attributes within the HDF5 file.
They work like directories/folder on a computer.
When using group, you can access their contents using absolute or relative paths.

```fortran
    use hdf5_utils
    
    integer(HID_T) :: file_id, group_id
    
    ! open file
    call hdf_open_file(file_id, "outfile.h5", STATUS='NEW')
    
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
    
```

Notice in the relative path case, we can pass the `group_id` to read/write subroutines instead of the `file_id`.
This can be used for convenient access to nested objects.


### Unkown size

There are two subroutine, `hdf_get_rank` and `hdf_get_dims`, to get the rank (tensor rank, number of dimensions) of a dataset and the dimensions  of a dataset.
These can be use, for example, to allocate an array before reading in the data

```fortran
    use hdf5_utils
    
    integer(HID_T) :: file_id
    integer :: rank, dims(6)
    integer, allocatable :: test4(:,:,:,:)
    
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
    
```


### Writing/reading by column

It is sometimes convenient to write/read only single column of a multidimensional array.

> **NOTE:** FORTRAN is a column oriented programming language.
> This means that elements of each column is stored contiguously in memory,
> and therefore has fastest access.
> The HDF5 library is written in C, which is a row oriented language.
> Since the FORTRAN library is just a wrapper around the C library,
> the array written out to the HDF5 file is the transpose of the array we see in FORTRAN.
> So we are actually reading in a 'row' along the last dimension in the dataset in the HDF5 file
> as a column vector in FORTRAN.
> Keep this in mind if you read access this data in another language,
> like C or Python.

For instance, if a 2d array is a set of data vectors that we wish to perform a long operation on, then we can read in one vector at a time and treat it before moving onto the next one.

```fortran
    use hdf5_utils

    integer(HID_T) :: file_id

    integer :: j, k
    real(dp) :: array(4)
    
    write(*,'(A)') ""
    write(*,'(A)') "Test writing out by column"
    
    ! open file
    call hdf_open_file(file_id, "test_hl.h5", STATUS='OLD', ACTION='WRITE')

    ! create dataset
    call hdf_create_dataset(file_id, "vdata3", (/4,6,8/), "double")

    ! loop through array
    do j = 1, 6
       do k = 1, 8
          array = real(j+k-1, dp)
          call hdf_write_vector_to_dataset(file_id, "vdata3", (/ j, k /), array)
       end do
    end do

    call hdf_read_vector_from_dataset(file_id, "data3", (/1,1/), array)
    write(*,*) array
    
    ! close file
    call hdf_close_file(file_id)

```
