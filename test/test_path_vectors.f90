!> CPython reference values for `parquet_utils`' path procedures.
!!
!! **GENERATED FILE -- DO NOT EDIT BY HAND.** Emitted by `tools/generate_path_reference.py`;
!! `tools/generate_path_reference.py --check` fails if this file has drifted from what the
!! generator produces, and CI runs it. **Never hand-edit a value here**: these rows are what
!! turn "we follow Python" into something checked rather than claimed, so an edited one is a
!! lie that nothing else would catch.
!!
!! Every value is `posixpath.join`, `posixpath.dirname`, `posixpath.basename` or
!! `posixpath.splitext` applied to the fixture beside it, by the CPython this was last
!! regenerated with. `posixpath` is named explicitly and `os.path` never is: the latter
!! resolves to `ntpath` on Windows, which would emit Windows rules from a Windows host.
!!
!! **Three of `parquet_utils`' rules are NOT here and must not be added.** `pf_join_path`
!! over a zero-size array, a blank-padded fixed-length `suffix`, and a component with
!! trailing blanks are Fortran-specific extensions of the Python contract -- CPython either
!! raises or has no equivalent -- so they are asserted by hand in `test/test_utils.f90`.
!! Putting them here would make it look as though CPython had been consulted about them.
module test_path_vectors
    implicit none
    public

    !> Join cases recorded.
    integer, parameter :: pv_n_join = 30
    !> Path-splitting cases recorded.
    integer, parameter :: pv_n_split = 26
    !> Suffix-insertion cases recorded.
    integer, parameter :: pv_n_suffix = 10

    !> How many of the five component slots each join case actually uses.
    integer, parameter :: pv_join_arity(*) = [ &
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, &
        3, 4, 4, 4, 4, 5, 5, 5, 5, 5 &
        ]

    !> Component 1 of each join case; blank past that case's arity.
    character(len=10), parameter :: pv_join_p1(*) = [ &
        character(len=10) :: "a", &
        "a/", &
        "a", &
        "a/", &
        "", &
        "a", &
        "a", &
        "./", &
        "/", &
        "a/b", &
        "a//", &
        "a", &
        " a", &
        "/", &
        "", &
        "a", &
        "a", &
        "a/", &
        "a", &
        "", &
        "/x", &
        "a", &
        "a", &
        "/", &
        "a", &
        "a", &
        "a/", &
        "", &
        "/a", &
        "x" &
        ]

    !> Component 2 of each join case; blank past that case's arity.
    character(len=10), parameter :: pv_join_p2(*) = [ &
        character(len=10) :: "b", &
        "b", &
        "/b", &
        "/b", &
        "b", &
        "", &
        "b/", &
        "b", &
        "b", &
        "../c", &
        "b", &
        "b//", &
        "b", &
        "/", &
        "", &
        "b", &
        "", &
        "b", &
        "/b", &
        "", &
        "y", &
        "b", &
        "", &
        "a", &
        "b", &
        "b", &
        "/b", &
        "a", &
        "b", &
        "y" &
        ]

    !> Component 3 of each join case; blank past that case's arity.
    character(len=10), parameter :: pv_join_p3(*) = [ &
        character(len=10) :: "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "c", &
        "b", &
        "c/", &
        "c", &
        "", &
        "z", &
        "c", &
        "", &
        "b", &
        "/c", &
        "c", &
        "c/", &
        "", &
        "c", &
        "z" &
        ]

    !> Component 4 of each join case; blank past that case's arity.
    character(len=10), parameter :: pv_join_p4(*) = [ &
        character(len=10) :: "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "d", &
        "d", &
        "c", &
        "d", &
        "d", &
        "d", &
        "b", &
        "d", &
        "w" &
        ]

    !> Component 5 of each join case; blank past that case's arity.
    character(len=10), parameter :: pv_join_p5(*) = [ &
        character(len=10) :: "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "e", &
        "", &
        "", &
        "e", &
        "/v" &
        ]

    !> What `posixpath.join` returns for each case.
    character(len=10), parameter :: pv_join_want(*) = [ &
        character(len=10) :: "a/b", &
        "a/b", &
        "/b", &
        "/b", &
        "b", &
        "a/", &
        "a/b/", &
        "./b", &
        "/b", &
        "a/b/../c", &
        "a//b", &
        "a/b//", &
        " a/b", &
        "/", &
        "", &
        "a/b/c", &
        "a/b", &
        "a/b/c/", &
        "/b/c", &
        "", &
        "/x/y/z", &
        "a/b/c/d", &
        "a/d", &
        "/a/b/c", &
        "/c/d", &
        "a/b/c/d/e", &
        "/b/c/d/", &
        "a/b/", &
        "/a/b/c/d/e", &
        "/v" &
        ]

    !> The path each splitting case is applied to.
    character(len=22), parameter :: pv_split_path(*) = [ &
        character(len=22) :: "/data/run3/cat.parquet", &
        "myfile.txt", &
        "/a/b/", &
        "/x", &
        "/", &
        "", &
        "./x", &
        ".bashrc", &
        "a.tar.gz", &
        "/a.b/c", &
        "a.", &
        "..", &
        "...", &
        ".a.b", &
        "a//b", &
        "a//b.txt", &
        "//", &
        "//a", &
        "dir/.hidden", &
        "dir/.hidden.txt", &
        "/data/", &
        "x/y/z.tar.gz", &
        " leading/space.txt", &
        "no_ext_at_all", &
        "/.", &
        "trailing.dot." &
        ]

    !> `posixpath.dirname` of each.
    character(len=22), parameter :: pv_split_dir(*) = [ &
        character(len=22) :: "/data/run3", &
        "", &
        "/a/b", &
        "/", &
        "/", &
        "", &
        ".", &
        "", &
        "", &
        "/a.b", &
        "", &
        "", &
        "", &
        "", &
        "a", &
        "a", &
        "//", &
        "//", &
        "dir", &
        "dir", &
        "/data", &
        "x/y", &
        " leading", &
        "", &
        "/", &
        "" &
        ]

    !> `posixpath.basename` of each.
    character(len=22), parameter :: pv_split_base(*) = [ &
        character(len=22) :: "cat.parquet", &
        "myfile.txt", &
        "", &
        "x", &
        "", &
        "", &
        "x", &
        ".bashrc", &
        "a.tar.gz", &
        "c", &
        "a.", &
        "..", &
        "...", &
        ".a.b", &
        "b", &
        "b.txt", &
        "", &
        "a", &
        ".hidden", &
        ".hidden.txt", &
        "", &
        "z.tar.gz", &
        "space.txt", &
        "no_ext_at_all", &
        ".", &
        "trailing.dot." &
        ]

    !> `posixpath.splitext(path)[1]` of each -- the extension, including its dot.
    character(len=22), parameter :: pv_split_ext(*) = [ &
        character(len=22) :: ".parquet", &
        ".txt", &
        "", &
        "", &
        "", &
        "", &
        "", &
        "", &
        ".gz", &
        "", &
        ".", &
        "", &
        "", &
        ".b", &
        "", &
        ".txt", &
        "", &
        "", &
        "", &
        ".txt", &
        "", &
        ".gz", &
        ".txt", &
        "", &
        "", &
        "." &
        ]

    !> `posixpath.splitext(posixpath.basename(path))[0]` of each.
    character(len=22), parameter :: pv_split_stem(*) = [ &
        character(len=22) :: "cat", &
        "myfile", &
        "", &
        "x", &
        "", &
        "", &
        "x", &
        ".bashrc", &
        "a.tar", &
        "c", &
        "a", &
        "..", &
        "...", &
        ".a", &
        "b", &
        "b", &
        "", &
        "a", &
        ".hidden", &
        ".hidden", &
        "", &
        "z.tar", &
        "space", &
        "no_ext_at_all", &
        ".", &
        "trailing.dot" &
        ]

    !> The path each suffix-insertion case is applied to.
    character(len=21), parameter :: pv_suffix_path(*) = [ &
        character(len=21) :: "/data/myfile.txt", &
        "myfile.txt", &
        "myfile", &
        "a.tar.gz", &
        ".bashrc", &
        "/a/b/", &
        "a//b.txt", &
        "/x", &
        "", &
        "dir/.hidden" &
        ]

    !> The suffix inserted before the extension.
    character(len=21), parameter :: pv_suffix_sfx(*) = [ &
        character(len=21) :: "_stat", &
        "_stat", &
        "_stat", &
        "_stat", &
        "_stat", &
        "_stat", &
        "_stat", &
        "_2", &
        "_x", &
        "_s" &
        ]

    !> The rebuilt path: prefix through the last separator, then stem, suffix, extension.
    character(len=21), parameter :: pv_suffix_want(*) = [ &
        character(len=21) :: "/data/myfile_stat.txt", &
        "myfile_stat.txt", &
        "myfile_stat", &
        "a.tar_stat.gz", &
        ".bashrc_stat", &
        "/a/b/_stat", &
        "a//b_stat.txt", &
        "/x_2", &
        "_x", &
        "dir/.hidden_s" &
        ]

    ! gcov attribution artifact: an `end module` line is not a statement and reports 0 hits.
end module test_path_vectors ! GCOVR_EXCL_LINE
