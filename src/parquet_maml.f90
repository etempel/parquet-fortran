module parquet_maml
    implicit none
    private

    type, public :: parquet_maml_file
        character(len=:), allocatable :: name
        character(len=:), allocatable :: lines(:)
    end type parquet_maml_file

    public :: get_parquet_maml, parquet_maml_maml_example, parquet_maml_maml_example2

contains

    function get_parquet_maml(name) result(maml)
        character(len=*), intent(in) :: name
        type(parquet_maml_file) :: maml

        select case (trim(name))
        case ("maml_example.maml")
            maml = parquet_maml_maml_example()
        case ("maml_example")
            maml = parquet_maml_maml_example()
        case ("maml_example2.maml")
            maml = parquet_maml_maml_example2()
        case ("maml_example2")
            maml = parquet_maml_maml_example2()
        case default
            error stop "get_parquet_maml: unknown internal MAML file: " // trim(name)
        end select
    end function get_parquet_maml

    function parquet_maml_maml_example() result(maml)
        type(parquet_maml_file) :: maml

        maml%name = "maml_example.maml"
        allocate(character(len=104) :: maml%lines(69))
        maml%lines = [ character(len=104) :: &
            "dataset: input_data", &
            "table: input_table", &
            "author: Dave Smith <dave_smith_is_not_here@gmail.com>", &
            "description: Just an example. Probably do not write tonnes here. A few sentences is usually about right.", &
            "comments:", &
            "- This is an example. Remember comments are lists.", &
            "- Another comment.", &
            "- zeropoint: 1.00", &
            "license: Copyright [Private]", &
            "fields:", &
            "- name: id0", &
            "  unit: unitless", &
            "  info: ID field.", &
            "  ucd: meta.id;meta.main", &
            "  data_type: int32", &
            "- name: idarr", &
            "  unit: unitless", &
            "  info: ID field.", &
            "  ucd: meta.id", &
            "  data_type: int64", &
            "  array_size: 2", &
            "- name: name", &
            "  unit: unitless", &
            "  info: Name of the object.", &
            "  ucd: meta.main", &
            "  data_type: string", &
            "- name: name_arr", &
            "  info: Name of the object.", &
            "  data_type: string", &
            "  array_size: 3", &
            "- name: idlong", &
            "  unit: count", &
            "  info: Long ID field.", &
            "  ucd: meta.id", &
            "  data_type: int64", &
            "- name: value", &
            "  unit: m/s", &
            "  info: Value of the object.", &
            "  ucd: phys.veloc;phys.speed", &
            "  data_type: float32", &
            "- name: value_64", &
            "  unit: m/s", &
            "  info: Second value of the object.", &
            "  ucd: phys.veloc;phys.speed", &
            "  data_type: float64", &
            "- name: arr", &
            "  unit: unitless", &
            "  info: Array of values.", &
            "  ucd: ", &
            "  data_type: float32", &
            "  array_size: 5", &
            "- name: arrlong", &
            "  unit: unitless", &
            "  info: Array of values.", &
            "  data_type: float64", &
            "  array_size: 5", &
            "- name: val", &
            "  info: Value of the object.", &
            "  data_type: float32", &
            "- name: iarr", &
            "  info: Array of values.", &
            "  data_type: int32", &
            "  array_size: 3", &
            "- name: myflag", &
            "  data_type: boolean", &
            "- name: flag_array", &
            "  info: Flag for the object.", &
            "  data_type: boolean", &
            "  array_size: 6" ]
    end function parquet_maml_maml_example
    function parquet_maml_maml_example2() result(maml)
        type(parquet_maml_file) :: maml

        maml%name = "maml_example2.maml"
        allocate(character(len=104) :: maml%lines(12))
        maml%lines = [ character(len=104) :: &
            "dataset: input_data", &
            "table: input_table", &
            "author: Dave Smith <dave_smith_is_not_here@gmail.com>", &
            "description: Just an example. Probably do not write tonnes here. A few sentences is usually about right.", &
            "license: Copyright [Private]", &
            "fields:", &
            "- name: id", &
            "  info: ID field.", &
            "  data_type: int32", &
            "- name: name", &
            "  info: Name of the object.", &
            "  data_type: string" ]
    end function parquet_maml_maml_example2

end module parquet_maml
