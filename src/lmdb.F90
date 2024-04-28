! lmdb.f90
!
! Author:  Philipp Engel
! Licence: ISC
module lmdb
    !! Auto-generated Fortran 2018 interface bindings to the OpenLDAP Lightning
    !! Memory-Mapped Database (LMDB).
    !!
    !! ## Preprocessor Flags
    !!
    !! If this module is built with GNU Fortran, specify additional
    !! preprocessor flags, depending on the operating system.
    !!
    !! On Linux:
    !!
    !! ```
    !! $ gfortran -D__linux__ -c lmdb.F90
    !! ```
    !!
    !! On FreeBSD:
    !!
    !! ```
    !! $ gfortran -D__FreeBSD__ -c lmdb.F90
    !! ```
    !!
    !! On MS Windows:
    !!
    !! ```
    !! $ gfortran -D_MSC_VER -D_WIN32 -c lmdb.F90
    !! ```
    !!
    !! ## C API
    !!
    !! * http://www.lmdb.tech/doc/
    !!
    use, intrinsic :: iso_c_binding
    implicit none (type, external)
    private

    integer, parameter :: c_uint16_t     = c_int16_t
    integer, parameter :: c_uint32_t     = c_int32_t
    integer, parameter :: c_unsigned_int = c_int

#if defined (__linux__)
    integer, parameter :: c_mode_t = c_uint32_t
#elif defined (__FreeBSD__)
    integer, parameter :: c_mode_t = c_uint16_t
#endif

#if defined (_MSC_VER)
    integer, parameter, public :: mdb_mode_t       = c_int
#else
    integer, parameter, public :: mdb_mode_t       = c_mode_t
    integer, parameter, public :: mdb_filehandle_t = c_int
#endif

    integer, parameter, public :: mdb_dbi = c_unsigned_int !! A handle for an individual database in the DB environment.

    ! Environment flags
    integer(kind=c_int), parameter, public :: MDB_FIXEDMAP    = int(z'01')      !! mmap at a fixed address (experimental).
    integer(kind=c_int), parameter, public :: MDB_NOSUBDIR    = int(z'4000')    !! No environment directory.
    integer(kind=c_int), parameter, public :: MDB_NOSYNC      = int(z'10000')   !! Don’t fsync after commit.
    integer(kind=c_int), parameter, public :: MDB_RDONLY      = int(z'20000')   !! Read only.
    integer(kind=c_int), parameter, public :: MDB_NOMETASYNC  = int(z'40000')   !! Don’t fsync metapage after commit.
    integer(kind=c_int), parameter, public :: MDB_WRITEMAP    = int(z'80000')   !! Use writable mmap.
    integer(kind=c_int), parameter, public :: MDB_MAPASYNC    = int(z'100000')  !! Use asynchronous msync when MDB_WRITEMAP is used.
    integer(kind=c_int), parameter, public :: MDB_NOTLS       = int(z'200000')  !! Tie reader locktable slots to MDB_txn objects instead of to threads.
    integer(kind=c_int), parameter, public :: MDB_NOLOCK      = int(z'400000')  !! Don’t do any locking, caller must manage their own locks.
    integer(kind=c_int), parameter, public :: MDB_NORDAHEAD   = int(z'800000')  !! Don’t do readahead (no effect on Windows).
    integer(kind=c_int), parameter, public :: MDB_NOMEMINIT   = int(z'1000000') !! Don’t initialize malloc'd memory before writing to datafile.

    ! Database flags
    integer(kind=c_int), parameter, public :: MDB_REVERSEKEY  = int(z'02')      !! Use reverse string keys.
    integer(kind=c_int), parameter, public :: MDB_DUPSORT     = int(z'04')      !! Use sorted duplicates.
    integer(kind=c_int), parameter, public :: MDB_INTEGERKEY  = int(z'08')      !! Numeric keys in native byte order: either unsigned int or size_t. The keys must all be of the same size.
    integer(kind=c_int), parameter, public :: MDB_DUPFIXED    = int(z'10')      !! With MDB_DUPSORT, sorted dup items have fixed size.
    integer(kind=c_int), parameter, public :: MDB_INTEGERDUP  = int(z'20')      !! With MDB_DUPSORT, dups are MDB_INTEGERKEY-style integers.
    integer(kind=c_int), parameter, public :: MDB_REVERSEDUP  = int(z'40')      !! With MDB_DUPSORT, use reverse string dups.
    integer(kind=c_int), parameter, public :: MDB_CREATE      = int(z'40000')   !! Create DB if not already existing.

    ! Write flags
    integer(kind=c_int), parameter, public :: MDB_NOOVERWRITE = int(z'10')      !! For put: Don’t write if the key already exists.
    integer(kind=c_int), parameter, public :: MDB_NODUPDATA   = int(z'20')      !! Only for MDB_DUPSORT. For put: don’t write if the key and data pair already exist. For mdb_cursor_del: remove all duplicate data items.
    integer(kind=c_int), parameter, public :: MDB_CURRENT     = int(z'40')      !! For mdb_cursor_put: overwrite the current key/data pair.
    integer(kind=c_int), parameter, public :: MDB_RESERVE     = int(z'10000')   !! For put: Just reserve space for data, don’t copy it. Return a pointer to the reserved space.
    integer(kind=c_int), parameter, public :: MDB_APPEND      = int(z'20000')   !! Data is being appended, don’t split full pages.
    integer(kind=c_int), parameter, public :: MDB_APPENDDUP   = int(z'40000')   !! Duplicate data is being appended, don’t split full pages.
    integer(kind=c_int), parameter, public :: MDB_MULTIPLE    = int(z'80000')   !! Store multiple data items in one call. Only for MDB_DUPFIXED.

    ! Copy flags
    integer(kind=c_int), parameter, public :: MDB_CP_COMPACT  = int(z'01')      !! Compacting copy: Omit free space from copy, and renumber all pages sequentially.

    ! MDB_cursor_op
    integer(kind=c_int), parameter, public :: MDB_FIRST          = 0            !! Position at first key/data item.
    integer(kind=c_int), parameter, public :: MDB_FIRST_DUP      = 1            !! Position at first data item of current key. Only for MDB_DUPSORT.
    integer(kind=c_int), parameter, public :: MDB_GET_BOTH       = 2            !! Position at key/data pair. Only for MDB_DUPSORT.
    integer(kind=c_int), parameter, public :: MDB_GET_BOTH_RANGE = 3            !! Position at key, nearest data. Only for MDB_DUPSORT.
    integer(kind=c_int), parameter, public :: MDB_GET_CURRENT    = 4            !! Return key/data at current cursor position.
    integer(kind=c_int), parameter, public :: MDB_GET_MULTIPLE   = 5            !! Return key and up to a page of duplicate data items from current cursor position. Move cursor to prepare for MDB_NEXT_MULTIPLE. Only for MDB_DUPFIXED.
    integer(kind=c_int), parameter, public :: MDB_LAST           = 6            !! Position at last key/data item.
    integer(kind=c_int), parameter, public :: MDB_LAST_DUP       = 7            !! Position at last data item of current key. Only for MDB_DUPSORT.
    integer(kind=c_int), parameter, public :: MDB_NEXT           = 8            !! Position at next data item.
    integer(kind=c_int), parameter, public :: MDB_NEXT_DUP       = 9            !! Position at next data item of current key. Only for MDB_DUPSORT.
    integer(kind=c_int), parameter, public :: MDB_NEXT_MULTIPLE  = 10           !! Return key and up to a page of duplicate data items from next cursor position. Move cursor to prepare for MDB_NEXT_MULTIPLE. Only for MDB_DUPFIXED.
    integer(kind=c_int), parameter, public :: MDB_NEXT_NODUP     = 11           !! Position at first data item of next key.
    integer(kind=c_int), parameter, public :: MDB_PREV           = 12           !! Position at previous data item.
    integer(kind=c_int), parameter, public :: MDB_PREV_DUP       = 13           !! Position at previous data item of current key. Only for MDB_DUPSORT.
    integer(kind=c_int), parameter, public :: MDB_PREV_NODUP     = 14           !! Position at last data item of previous key.
    integer(kind=c_int), parameter, public :: MDB_SET            = 15           !! Position at specified key.
    integer(kind=c_int), parameter, public :: MDB_SET_KEY        = 16           !! Position at specified key, return key + data.
    integer(kind=c_int), parameter, public :: MDB_SET_RANGE      = 17           !! Position at first key greater than or equal to specified key.
    integer(kind=c_int), parameter, public :: MDB_PREV_MULTIPLE  = 18           !! Position at previous page and return up to a page of duplicate data items. Only for MDB_DUPFIXED.

    ! Return codes
    integer(kind=c_int), parameter, public :: MDB_SUCCESS          = 0          !! Successful result.
    integer(kind=c_int), parameter, public :: MDB_KEYEXIST         = -30799     !! Key/data pair already exists.
    integer(kind=c_int), parameter, public :: MDB_NOTFOUND         = -30798     !! Key/data pair not found (EOF).
    integer(kind=c_int), parameter, public :: MDB_PAGE_NOTFOUND    = -30797     !! Requested page not found – this usually indicates corruption.
    integer(kind=c_int), parameter, public :: MDB_CORRUPTED        = -30796     !! Located page was wrong type.
    integer(kind=c_int), parameter, public :: MDB_PANIC            = -30795     !! Update of meta page failed or environment had fatal error.
    integer(kind=c_int), parameter, public :: MDB_VERSION_MISMATCH = -30794     !! Environment version mismatch.
    integer(kind=c_int), parameter, public :: MDB_INVALID          = -30793     !! File is not a valid LMDB file.
    integer(kind=c_int), parameter, public :: MDB_MAP_FULL         = -30792     !! Environment mapsize reached.
    integer(kind=c_int), parameter, public :: MDB_DBS_FULL         = -30791     !! Environment maxdbs reached.
    integer(kind=c_int), parameter, public :: MDB_READERS_FULL     = -30790     !! Environment maxreaders reached.
    integer(kind=c_int), parameter, public :: MDB_TLS_FULL         = -30789     !! Too many TLS keys in use – Windows only.
    integer(kind=c_int), parameter, public :: MDB_TXN_FULL         = -30788     !! Txn has too many dirty pages.
    integer(kind=c_int), parameter, public :: MDB_CURSOR_FULL      = -30787     !! Cursor stack too deep – internal error.
    integer(kind=c_int), parameter, public :: MDB_PAGE_FULL        = -30786     !! Page has not enough space – internal error.
    integer(kind=c_int), parameter, public :: MDB_MAP_RESIZED      = -30785     !! Database contents grew beyond environment mapsize.
    integer(kind=c_int), parameter, public :: MDB_INCOMPATIBLE     = -30784     !! Operation and DB incompatible, or DB type changed.
    integer(kind=c_int), parameter, public :: MDB_BAD_RSLOT        = -30783     !! Invalid reuse of reader locktable slot.
    integer(kind=c_int), parameter, public :: MDB_BAD_TXN          = -30782     !! Transaction must abort, has a child, or is invalid.
    integer(kind=c_int), parameter, public :: MDB_BAD_VALSIZE      = -30781     !! Unsupported size of key/DB name/data, or wrong DUPFIXED size.
    integer(kind=c_int), parameter, public :: MDB_BAD_DBI          = -30780     !! The specified DBI was changed unexpectedly.

    integer(kind=c_int), parameter, public :: MDB_LAST_ERRCODE     = MDB_BAD_DBI

    ! MDB_val
    type, bind(c), public :: mdb_val_type
        !! Generic structure used for passing keys and data in and out of the database.
        integer(kind=c_size_t) :: mv_size = 0
        type(c_ptr)            :: mv_data = c_null_ptr
    end type mdb_val_type

    ! MDB_stat
    type, bind(c), public :: mdb_stat_type
        !! Statistics for a database in the environment.
        integer(kind=c_unsigned_int) :: ms_psize          = 0
        integer(kind=c_unsigned_int) :: ms_depth          = 0
        integer(kind=c_size_t)       :: ms_branch_pages   = 0
        integer(kind=c_size_t)       :: ms_leaf_pages     = 0
        integer(kind=c_size_t)       :: ms_overflow_pages = 0
        integer(kind=c_size_t)       :: ms_entries        = 0
    end type mdb_stat_type

    ! MDB_envinfo
    type, bind(c), public :: mdb_envinfo_type
        !! Information about the environment.
        type(c_ptr)                  :: me_mapaddr    = c_null_ptr
        integer(kind=c_size_t)       :: me_mapsize    = 0
        integer(kind=c_size_t)       :: me_last_pgno  = 0
        integer(kind=c_size_t)       :: me_last_txnid = 0
        integer(kind=c_unsigned_int) :: me_maxreaders = 0
        integer(kind=c_unsigned_int) :: me_numreaders = 0
    end type mdb_envinfo_type

    public :: c_f_str_ptr

    public :: mdb_assert_func
    public :: mdb_cmp_func
    public :: mdb_msg_func
    public :: mdb_rel_func

    public :: mdb_cmp
    public :: mdb_cursor_close
    public :: mdb_cursor_count
    public :: mdb_cursor_dbi
    public :: mdb_cursor_del
    public :: mdb_cursor_get
    public :: mdb_cursor_open
    public :: mdb_cursor_put
    public :: mdb_cursor_renew
    public :: mdb_cursor_txn
    public :: mdb_dbi_close
    public :: mdb_dbi_flags
    public :: mdb_dbi_open
    public :: mdb_dbi_open_
    public :: mdb_dcmp
    public :: mdb_del
    public :: mdb_drop
    public :: mdb_env_close
    public :: mdb_env_close_
    public :: mdb_env_copy
    public :: mdb_env_copy2
    public :: mdb_env_copyfd
    public :: mdb_env_copyfd2
    public :: mdb_env_create
    public :: mdb_env_get_fd
    public :: mdb_env_get_flags
    public :: mdb_env_get_maxkeysize
    public :: mdb_env_get_maxreaders
    public :: mdb_env_get_path
    public :: mdb_env_get_userctx
    public :: mdb_env_info
    public :: mdb_env_open
    public :: mdb_env_open_
    public :: mdb_env_set_assert
    public :: mdb_env_set_assert_
    public :: mdb_env_set_flags
    public :: mdb_env_set_mapsize
    public :: mdb_env_set_maxdbs
    public :: mdb_env_set_maxreaders
    public :: mdb_env_set_userctx
    public :: mdb_env_stat
    public :: mdb_env_sync
    public :: mdb_get
    public :: mdb_put
    public :: mdb_reader_check
    public :: mdb_reader_list
    public :: mdb_reader_list_
    public :: mdb_set_compare
    public :: mdb_set_compare_
    public :: mdb_set_dupsort
    public :: mdb_set_dupsort_
    public :: mdb_set_relctx
    public :: mdb_set_relfunc
    public :: mdb_stat
    public :: mdb_strerror
    public :: mdb_strerror_
    public :: mdb_txn_abort
    public :: mdb_txn_abort_
    public :: mdb_txn_begin
    public :: mdb_txn_commit
    public :: mdb_txn_env
    public :: mdb_txn_id
    public :: mdb_txn_renew
    public :: mdb_txn_reset
    public :: mdb_version
    public :: mdb_version_

    abstract interface
        ! void MDB_assert_func(MDB_env *env, const char *msg)
        subroutine mdb_assert_func(env, msg) bind(c)
            !! A callback function for most LMDB assert() failures, called
            !! before printing the message and aborting.
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: env
            type(c_ptr), intent(in), value :: msg
        end subroutine mdb_assert_func

        ! int MDB_cmp_func(const MDB_val *a, const MDB_val *b)
        function mdb_cmp_func(a, b) bind(c)
            !! A callback function used to compare two keys in a database.
            import :: c_int, mdb_val_type
            implicit none
            type(mdb_val_type), intent(inout) :: a
            type(mdb_val_type), intent(inout) :: b
            integer(kind=c_int)               :: mdb_cmp_func
        end function mdb_cmp_func

        ! int MDB_msg_func(const char *msg, void *ctx)
        function mdb_msg_func(msg, ctx) bind(c)
            !! A callback function used to print a message from the library.
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: msg
            type(c_ptr), intent(in), value :: ctx
            integer(kind=c_int)            :: mdb_msg_func
        end function mdb_msg_func

        ! void MDB_rel_func(MDB_val *item, void *oldptr, void *newptr, void *relctx)
        subroutine mdb_rel_func(item, old_ptr, new_ptr, rel_ctx) bind(c)
            !! A callback function used to relocate a position-dependent data
            !! item in a fixed-address database.
            import :: c_ptr, mdb_val_type
            implicit none
            type(mdb_val_type), intent(inout)     :: item
            type(c_ptr),        intent(in), value :: old_ptr
            type(c_ptr),        intent(in), value :: new_ptr
            type(c_ptr),        intent(in), value :: rel_ctx
        end subroutine mdb_rel_func
    end interface

    interface
        ! int mdb_cmp(MDB_txn *txn, MDB_dbi dbi, const MDB_val *a, const MDB_val *b)
        function mdb_cmp(txn, dbi, a, b) bind(c, name='mdb_cmp')
            import :: c_int, c_ptr, mdb_dbi, mdb_val_type
            implicit none
            type(c_ptr),           intent(in), value :: txn
            integer(kind=mdb_dbi), intent(in), value :: dbi
            type(mdb_val_type),    intent(in)        :: a
            type(mdb_val_type),    intent(in)        :: b
            integer(kind=c_int)                      :: mdb_cmp
        end function mdb_cmp

        ! void mdb_cursor_close(MDB_cursor *cursor)
        subroutine mdb_cursor_close(cursor) bind(c, name='mdb_cursor_close')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: cursor
        end subroutine mdb_cursor_close

        ! int mdb_cursor_count(MDB_cursor *cursor, size_t *countp)
        function mdb_cursor_count(cursor, countp) bind(c, name='mdb_cursor_count')
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),            intent(in), value :: cursor
            integer(kind=c_size_t), intent(out)       :: countp
            integer(kind=c_int)                       :: mdb_cursor_count
        end function mdb_cursor_count

        ! MDB_dbi mdb_cursor_dbi(MDB_cursor *cursor)
        function mdb_cursor_dbi(cursor) bind(c, name='mdb_cursor_dbi')
            import :: c_ptr, mdb_dbi
            implicit none
            type(c_ptr), intent(in), value :: cursor
            integer(kind=mdb_dbi)          :: mdb_cursor_dbi
        end function mdb_cursor_dbi

        ! int mdb_cursor_del(MDB_cursor *cursor, unsigned int flags)
        function mdb_cursor_del(cursor, flags) bind(c, name='mdb_cursor_del')
            import :: c_int, c_ptr, c_unsigned_int
            implicit none
            type(c_ptr), intent(in), value :: cursor
            integer(kind=c_unsigned_int), intent(in), value :: flags
            integer(kind=c_int) :: mdb_cursor_del
        end function mdb_cursor_del

        ! int mdb_cursor_get(MDB_cursor *cursor, MDB_val *key, MDB_val *data, MDB_cursor_op op)
        function mdb_cursor_get(cursor, key, data, op) bind(c, name='mdb_cursor_get')
            import :: c_int, c_ptr, mdb_val_type
            implicit none
            type(c_ptr),         intent(in), value :: cursor
            type(mdb_val_type),  intent(inout)     :: key
            type(mdb_val_type),  intent(inout)     :: data
            integer(kind=c_int), intent(in), value :: op
            integer(kind=c_int)                    :: mdb_cursor_get
        end function mdb_cursor_get

        ! int mdb_cursor_open(MDB_txn *txn, MDB_dbi dbi, MDB_cursor **cursor)
        function mdb_cursor_open(txn, dbi, cursor) bind(c, name='mdb_cursor_open')
            import :: c_int, c_ptr, mdb_dbi
            implicit none
            type(c_ptr),           intent(in), value :: txn
            integer(kind=mdb_dbi), intent(in), value :: dbi
            type(c_ptr),           intent(out)       :: cursor
            integer(kind=c_int)                      :: mdb_cursor_open
        end function mdb_cursor_open

        ! int mdb_cursor_put(MDB_cursor *cursor, MDB_val *key, MDB_val *data, unsigned int flags)
        function mdb_cursor_put(cursor, key, data, flags) bind(c, name='mdb_cursor_put')
            import :: c_int, c_ptr, c_unsigned_int, mdb_val_type
            implicit none
            type(c_ptr),                  intent(in), value :: cursor
            type(mdb_val_type),           intent(inout)     :: key
            type(mdb_val_type),           intent(inout)     :: data
            integer(kind=c_unsigned_int), intent(in), value :: flags
            integer(kind=c_int)                             :: mdb_cursor_put
        end function mdb_cursor_put

        ! int mdb_cursor_renew(MDB_txn *txn, MDB_cursor *cursor)
        function mdb_cursor_renew(txn, cursor) bind(c, name='mdb_cursor_renew')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: txn
            type(c_ptr), intent(in), value :: cursor
            integer(kind=c_int)            :: mdb_cursor_renew
        end function mdb_cursor_renew

        ! MDB_txn *mdb_cursor_txn(MDB_cursor *cursor)
        function mdb_cursor_txn(cursor) bind(c, name='mdb_cursor_txn')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: cursor
            type(c_ptr)                    :: mdb_cursor_txn
        end function mdb_cursor_txn

        ! void mdb_dbi_close(MDB_env *env, MDB_dbi dbi)
        subroutine mdb_dbi_close(env, dbi) bind(c, name='mdb_dbi_close')
            import :: c_ptr, mdb_dbi
            implicit none
            type(c_ptr),           intent(in), value :: env
            integer(kind=mdb_dbi), intent(in), value :: dbi
        end subroutine mdb_dbi_close

        ! int mdb_dbi_flags(MDB_txn *txn, MDB_dbi dbi, unsigned int *flags)
        function mdb_dbi_flags(txn, dbi, flags) bind(c, name='mdb_dbi_flags')
            import :: c_int, c_ptr, c_unsigned_int, mdb_dbi
            implicit none
            type(c_ptr),                  intent(in), value :: txn
            integer(kind=mdb_dbi),        intent(in), value :: dbi
            integer(kind=c_unsigned_int), intent(out)       :: flags
            integer(kind=c_int)                             :: mdb_dbi_flags
        end function mdb_dbi_flags

        ! int mdb_dbi_open(MDB_txn *txn, const char *name, unsigned int flags, MDB_dbi *dbi)
        function mdb_dbi_open_(txn, name, flags, dbi) bind(c, name='mdb_dbi_open')
            import :: c_char, c_int, c_ptr, c_unsigned_int, mdb_dbi
            implicit none
            type(c_ptr),                  intent(in), value :: txn
            character(kind=c_char),       intent(in)        :: name
            integer(kind=c_unsigned_int), intent(in), value :: flags
            integer(kind=mdb_dbi),        intent(out)       :: dbi
            integer(kind=c_int)                             :: mdb_dbi_open_
        end function mdb_dbi_open_

        ! int mdb_dcmp(MDB_txn *txn, MDB_dbi dbi, const MDB_val *a, const MDB_val *b)
        function mdb_dcmp(txn, dbi, a, b) bind(c, name='mdb_dcmp')
            import :: c_int, c_ptr, mdb_dbi, mdb_val_type
            implicit none
            type(c_ptr),           intent(in), value :: txn
            integer(kind=mdb_dbi), intent(in), value :: dbi
            type(mdb_val_type),    intent(in)        :: a
            type(mdb_val_type),    intent(in)        :: b
            integer(kind=c_int)                      :: mdb_dcmp
        end function mdb_dcmp

        ! int mdb_del(MDB_txn *txn, MDB_dbi dbi, MDB_val *key, MDB_val *data)
        function mdb_del(txn, dbi, key, data) bind(c, name='mdb_del')
            import :: c_int, c_ptr, mdb_dbi, mdb_val_type
            implicit none
            type(c_ptr),           intent(in), value :: txn
            integer(kind=mdb_dbi), intent(in), value :: dbi
            type(mdb_val_type),    intent(inout)     :: key
            type(c_ptr),           intent(in), value :: data
            integer(kind=c_int)                      :: mdb_del
        end function mdb_del

        ! int mdb_drop(MDB_txn *txn, MDB_dbi dbi, int del)
        function mdb_drop(txn, dbi, del) bind(c, name='mdb_drop')
            import :: c_int, c_ptr, mdb_dbi
            implicit none
            type(c_ptr),           intent(in), value :: txn
            integer(kind=mdb_dbi), intent(in), value :: dbi
            integer(kind=c_int),   intent(in), value :: del
            integer(kind=c_int)                      :: mdb_drop
        end function mdb_drop

        ! void mdb_env_close(MDB_env *env)
        subroutine mdb_env_close_(env) bind(c, name='mdb_env_close')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: env
        end subroutine mdb_env_close_

        ! int mdb_env_copy(MDB_env *env, const char *path)
        function mdb_env_copy(env, path) bind(c, name='mdb_env_copy')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: env
            character(kind=c_char), intent(in)        :: path
            integer(kind=c_int)                       :: mdb_env_copy
        end function mdb_env_copy

        ! int mdb_env_copy2(MDB_env *env, const char *path, unsigned int flags)
        function mdb_env_copy2(env, path, flags) bind(c, name='mdb_env_copy2')
            import :: c_char, c_int, c_ptr, c_unsigned_int
            implicit none
            type(c_ptr),                  intent(in), value :: env
            character(kind=c_char),       intent(in)        :: path
            integer(kind=c_unsigned_int), intent(in), value :: flags
            integer(kind=c_int)                             :: mdb_env_copy2
        end function mdb_env_copy2

        ! int mdb_env_create(MDB_env **env)
        function mdb_env_create(env) bind(c, name='mdb_env_create')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(out) :: env
            integer(kind=c_int)      :: mdb_env_create
        end function mdb_env_create

        ! int mdb_env_get_flags(MDB_env *env, unsigned int *flags)
        function mdb_env_get_flags(env, flags) bind(c, name='mdb_env_get_flags')
            import :: c_int, c_ptr, c_unsigned_int
            implicit none
            type(c_ptr),                  intent(in), value :: env
            integer(kind=c_unsigned_int), intent(out)       :: flags
            integer(kind=c_int)                             :: mdb_env_get_flags
        end function mdb_env_get_flags

        ! int mdb_env_get_maxkeysize(MDB_env *env)
        function mdb_env_get_maxkeysize(env) bind(c, name='mdb_env_get_maxkeysize')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: env
            integer(kind=c_int)            :: mdb_env_get_maxkeysize
        end function mdb_env_get_maxkeysize

        ! int mdb_env_get_maxreaders(MDB_env *env, unsigned int *readers)
        function mdb_env_get_maxreaders(env, readers) bind(c, name='mdb_env_get_maxreaders')
            import :: c_int, c_ptr, c_unsigned_int
            implicit none
            type(c_ptr),                  intent(in), value :: env
            integer(kind=c_unsigned_int), intent(out)       :: readers
            integer(kind=c_int)                             :: mdb_env_get_maxreaders
        end function mdb_env_get_maxreaders

        ! int mdb_env_get_path(MDB_env *env, const char **path)
        function mdb_env_get_path(env, path) bind(c, name='mdb_env_get_path')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: env
            type(c_ptr), intent(out)       :: path
            integer(kind=c_int)            :: mdb_env_get_path
        end function mdb_env_get_path

        ! void *mdb_env_get_userctx(MDB_env *env)
        function mdb_env_get_userctx(env) bind(c, name='mdb_env_get_userctx')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: env
            type(c_ptr)                    :: mdb_env_get_userctx
        end function mdb_env_get_userctx

        ! int mdb_env_info(MDB_env *env, MDB_envinfo *stat)
        function mdb_env_info(env, stat) bind(c, name='mdb_env_info')
            import :: c_int, c_ptr, mdb_envinfo_type
            implicit none
            type(c_ptr),            intent(in), value :: env
            type(mdb_envinfo_type), intent(inout)     :: stat
            integer(kind=c_int)                       :: mdb_env_info
        end function mdb_env_info

        ! int mdb_env_open(MDB_env *env, const char *path, unsigned int flags, mdb_mode_t mode)
        function mdb_env_open_(env, path, flags, mode) bind(c, name='mdb_env_open')
            import :: c_char, c_int, c_ptr, c_unsigned_int, mdb_mode_t
            implicit none
            type(c_ptr),                  intent(in), value :: env
            character(kind=c_char),       intent(in)        :: path
            integer(kind=c_unsigned_int), intent(in), value :: flags
            integer(kind=mdb_mode_t),     intent(in), value :: mode
            integer(kind=c_int)                             :: mdb_env_open_
        end function mdb_env_open_

        ! int mdb_env_set_assert(MDB_env *env, MDB_assert_func *func)
        function mdb_env_set_assert_(env, func) bind(c, name='mdb_env_set_assert')
            import :: c_funptr, c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: env
            type(c_funptr), intent(in), value :: func
            integer(kind=c_int)               :: mdb_env_set_assert_
        end function mdb_env_set_assert_

        ! int mdb_env_set_flags(MDB_env *env, unsigned int flags, int onoff)
        function mdb_env_set_flags(env, flags, onoff) bind(c, name='mdb_env_set_flags')
            import :: c_int, c_ptr, c_unsigned_int
            implicit none
            type(c_ptr),                  intent(in), value :: env
            integer(kind=c_unsigned_int), intent(in), value :: flags
            integer(kind=c_int),          intent(in), value :: onoff
            integer(kind=c_int)                             :: mdb_env_set_flags
        end function mdb_env_set_flags

        ! int mdb_env_set_mapsize(MDB_env *env, size_t size)
        function mdb_env_set_mapsize(env, size) bind(c, name='mdb_env_set_mapsize')
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),            intent(in), value :: env
            integer(kind=c_size_t), intent(in), value :: size
            integer(kind=c_int)                       :: mdb_env_set_mapsize
        end function mdb_env_set_mapsize

        ! int mdb_env_set_maxdbs(MDB_env *env, MDB_dbi dbs)
        function mdb_env_set_maxdbs(env, dbs) bind(c, name='mdb_env_set_maxdbs')
            import :: c_int, c_ptr, mdb_dbi
            implicit none
            type(c_ptr),           intent(in), value :: env
            integer(kind=mdb_dbi), intent(in), value :: dbs
            integer(kind=c_int)                      :: mdb_env_set_maxdbs
        end function mdb_env_set_maxdbs

        ! int mdb_env_set_maxreaders(MDB_env *env, unsigned int readers)
        function mdb_env_set_maxreaders(env, readers) bind(c, name='mdb_env_set_maxreaders')
            import :: c_int, c_ptr, c_unsigned_int
            implicit none
            type(c_ptr),                  intent(in), value :: env
            integer(kind=c_unsigned_int), intent(in), value :: readers
            integer(kind=c_int)                             :: mdb_env_set_maxreaders
        end function mdb_env_set_maxreaders

        ! int mdb_env_set_userctx(MDB_env *env, void *ctx)
        function mdb_env_set_userctx(env, ctx) bind(c, name='mdb_env_set_userctx')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: env
            type(c_ptr), intent(in), value :: ctx
            integer(kind=c_int)            :: mdb_env_set_userctx
        end function mdb_env_set_userctx

        ! int mdb_env_stat(MDB_env *env, MDB_stat *stat)
        function mdb_env_stat(env, stat) bind(c, name='mdb_env_stat')
            import :: c_int, c_ptr, mdb_stat_type
            implicit none
            type(c_ptr),         intent(in), value :: env
            type(mdb_stat_type), intent(out)       :: stat
            integer(kind=c_int)                    :: mdb_env_stat
        end function mdb_env_stat

        ! int mdb_env_sync(MDB_env *env, int force)
        function mdb_env_sync(env, force) bind(c, name='mdb_env_sync')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: env
            integer(kind=c_int), intent(in), value :: force
            integer(kind=c_int)                    :: mdb_env_sync
        end function mdb_env_sync

        ! int mdb_get(MDB_txn *txn, MDB_dbi dbi, MDB_val *key, MDB_val *data)
        function mdb_get(txn, dbi, key, data) bind(c, name='mdb_get')
            import :: c_int, c_ptr, mdb_dbi, mdb_val_type
            implicit none
            type(c_ptr),           intent(in), value :: txn
            integer(kind=mdb_dbi), intent(in), value :: dbi
            type(mdb_val_type),    intent(inout)     :: key
            type(mdb_val_type),    intent(inout)     :: data
            integer(kind=c_int)                      :: mdb_get
        end function mdb_get

        ! int mdb_put(MDB_txn *txn, MDB_dbi dbi, MDB_val *key, MDB_val *data, unsigned int flags)
        function mdb_put(txn, dbi, key, data, flags) bind(c, name='mdb_put')
            import :: c_int, c_ptr, c_unsigned_int, mdb_dbi, mdb_val_type
            implicit none
            type(c_ptr),                  intent(in), value :: txn
            integer(kind=mdb_dbi),        intent(in), value :: dbi
            type(mdb_val_type),           intent(inout)     :: key
            type(mdb_val_type),           intent(inout)     :: data
            integer(kind=c_unsigned_int), intent(in), value :: flags
            integer(kind=c_int)                             :: mdb_put
        end function mdb_put

        ! int mdb_reader_check(MDB_env *env, int *dead)
        function mdb_reader_check(env, dead) bind(c, name='mdb_reader_check')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: env
            integer(kind=c_int), intent(out)       :: dead
            integer(kind=c_int)                    :: mdb_reader_check
        end function mdb_reader_check

        ! int mdb_reader_list(MDB_env *env, MDB_msg_func *func, void *ctx)
        function mdb_reader_list_(env, func, ctx) bind(c, name='mdb_reader_list')
            import :: c_funptr, c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: env
            type(c_funptr), intent(in), value :: func
            type(c_ptr),    intent(in), value :: ctx
            integer(kind=c_int)               :: mdb_reader_list_
        end function mdb_reader_list_

        ! int mdb_set_compare(MDB_txn *txn, MDB_dbi dbi, MDB_cmp_func *cmp)
        function mdb_set_compare_(txn, dbi, cmp) bind(c, name='mdb_set_compare')
            import :: c_funptr, c_int, c_ptr, mdb_dbi
            implicit none
            type(c_ptr),           intent(in), value :: txn
            integer(kind=mdb_dbi), intent(in), value :: dbi
            type(c_funptr),        intent(in), value :: cmp
            integer(kind=c_int)                      :: mdb_set_compare_
        end function mdb_set_compare_

        ! int mdb_set_dupsort(MDB_txn *txn, MDB_dbi dbi, MDB_cmp_func *cmp)
        function mdb_set_dupsort_(txn, dbi, cmp) bind(c, name='mdb_set_dupsort')
            import :: c_funptr, c_int, c_ptr, mdb_dbi
            implicit none
            type(c_ptr),           intent(in), value :: txn
            integer(kind=mdb_dbi), intent(in), value :: dbi
            type(c_funptr),        intent(in), value :: cmp
            integer(kind=c_int)                      :: mdb_set_dupsort_
        end function mdb_set_dupsort_

        ! int mdb_set_relctx(MDB_txn *txn, MDB_dbi dbi, void *ctx)
        function mdb_set_relctx(txn, dbi, ctx) bind(c, name='mdb_set_relctx')
            import :: c_int, c_ptr, mdb_dbi
            implicit none
            type(c_ptr),           intent(in), value :: txn
            integer(kind=mdb_dbi), intent(in), value :: dbi
            type(c_ptr),           intent(in), value :: ctx
            integer(kind=c_int)                      :: mdb_set_relctx
        end function mdb_set_relctx

        ! int mdb_set_relfunc(MDB_txn *txn, MDB_dbi dbi, MDB_rel_func *rel)
        function mdb_set_relfunc_(txn, dbi, rel) bind(c, name='mdb_set_relfunc')
            import :: c_funptr, c_int, c_ptr, mdb_dbi
            implicit none
            type(c_ptr),           intent(in), value :: txn
            integer(kind=mdb_dbi), intent(in), value :: dbi
            type(c_funptr),        intent(in), value :: rel
            integer(kind=c_int)                      :: mdb_set_relfunc_
        end function mdb_set_relfunc_

        ! int mdb_stat(MDB_txn *txn, MDB_dbi dbi, MDB_stat *stat)
        function mdb_stat(txn, dbi, stat) bind(c, name='mdb_stat')
            import :: c_int, c_ptr, mdb_dbi, mdb_stat_type
            implicit none
            type(c_ptr),           intent(in), value :: txn
            integer(kind=mdb_dbi), intent(in), value :: dbi
            type(mdb_stat_type),   intent(out)       :: stat
            integer(kind=c_int)                      :: mdb_stat
        end function mdb_stat

        ! char *mdb_strerror(int err)
        function mdb_strerror_(err) bind(c, name='mdb_strerror')
            import :: c_int, c_ptr
            implicit none
            integer(kind=c_int), intent(in), value :: err
            type(c_ptr)                            :: mdb_strerror_
        end function mdb_strerror_

        ! void mdb_txn_abort(MDB_txn *txn)
        subroutine mdb_txn_abort_(txn) bind(c, name='mdb_txn_abort')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: txn
        end subroutine mdb_txn_abort_

        ! int mdb_txn_begin(MDB_env *env, MDB_txn *parent, unsigned int flags, MDB_txn **txn)
        function mdb_txn_begin(env, parent, flags, txn) bind(c, name='mdb_txn_begin')
            import :: c_int, c_ptr, c_unsigned_int
            implicit none
            type(c_ptr),                  intent(in), value :: env
            type(c_ptr),                  intent(in), value :: parent
            integer(kind=c_unsigned_int), intent(in), value :: flags
            type(c_ptr),                  intent(out)       :: txn
            integer(kind=c_int)                             :: mdb_txn_begin
        end function mdb_txn_begin

        ! int mdb_txn_commit(MDB_txn *txn)
        function mdb_txn_commit(txn) bind(c, name='mdb_txn_commit')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: txn
            integer(kind=c_int)            :: mdb_txn_commit
        end function mdb_txn_commit

        ! MDB_env *mdb_txn_env(MDB_txn *txn)
        function mdb_txn_env(txn) bind(c, name='mdb_txn_env')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: txn
            type(c_ptr)                    :: mdb_txn_env
        end function mdb_txn_env

        ! size_t mdb_txn_id(MDB_txn *txn)
        function mdb_txn_id(txn) bind(c, name='mdb_txn_id')
            import :: c_ptr, c_size_t
            implicit none
            type(c_ptr), intent(in), value :: txn
            integer(kind=c_size_t)         :: mdb_txn_id
        end function mdb_txn_id

        ! int mdb_txn_renew(MDB_txn *txn)
        function mdb_txn_renew(txn) bind(c, name='mdb_txn_renew')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: txn
            integer(kind=c_int)            :: mdb_txn_renew
        end function mdb_txn_renew

        ! void mdb_txn_reset(MDB_txn *txn)
        subroutine mdb_txn_reset(txn) bind(c, name='mdb_txn_reset')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: txn
        end subroutine mdb_txn_reset

        ! char *mdb_version(int *major, int *minor, int *patch)
        function mdb_version_(major, minor, patch) bind(c, name='mdb_version')
            import :: c_int, c_ptr
            implicit none
            integer(kind=c_int), intent(out) :: major
            integer(kind=c_int), intent(out) :: minor
            integer(kind=c_int), intent(out) :: patch
            type(c_ptr)                      :: mdb_version_
        end function mdb_version_
    end interface

#ifdef _WIN32
    interface
        ! int mdb_env_copyfd(MDB_env *env, mdb_filehandle_t fd)
        function mdb_env_copyfd(env, fd) bind(c, name='mdb_env_copyfd')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: env
            type(c_ptr), intent(in), value :: fd
            integer(kind=c_int)            :: mdb_env_copyfd
        end function mdb_env_copyfd

        ! int mdb_env_copyfd2(MDB_env *env, mdb_filehandle_t fd, unsigned int flags)
        function mdb_env_copyfd2(env, fd, flags) bind(c, name='mdb_env_copyfd2')
            import :: c_int, c_ptr, c_unsigned_int
            implicit none
            type(c_ptr),                  intent(in), value :: env
            type(c_ptr),                  intent(in), value :: fd
            integer(kind=c_unsigned_int), intent(in), value :: flags
            integer(kind=c_int)                             :: mdb_env_copyfd2
        end function mdb_env_copyfd2

        ! int mdb_env_get_fd(MDB_env *env, mdb_filehandle_t *fd)
        function mdb_env_get_fd(env, fd) bind(c, name='mdb_env_get_fd')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: env
            type(c_ptr), intent(out)       :: fd
            integer(kind=c_int)            :: mdb_env_get_fd
        end function mdb_env_get_fd
    end interface
#else
    interface
        ! int mdb_env_copyfd(MDB_env *env, mdb_filehandle_t fd)
        function mdb_env_copyfd(env, fd) bind(c, name='mdb_env_copyfd')
            import :: c_int, c_ptr, mdb_filehandle_t
            implicit none
            type(c_ptr),                    intent(in), value :: env
            integer(kind=mdb_filehandle_t), intent(in), value :: fd
            integer(kind=c_int)                               :: mdb_env_copyfd
        end function mdb_env_copyfd

        ! int mdb_env_copyfd2(MDB_env *env, mdb_filehandle_t fd, unsigned int flags)
        function mdb_env_copyfd2(env, fd, flags) bind(c, name='mdb_env_copyfd2')
            import :: c_int, c_ptr, c_unsigned_int, mdb_filehandle_t
            implicit none
            type(c_ptr),                    intent(in), value :: env
            integer(kind=mdb_filehandle_t), intent(in), value :: fd
            integer(kind=c_unsigned_int),   intent(in), value :: flags
            integer(kind=c_int)                               :: mdb_env_copyfd2
        end function mdb_env_copyfd2

        ! int mdb_env_get_fd(MDB_env *env, mdb_filehandle_t *fd)
        function mdb_env_get_fd(env, fd) bind(c, name='mdb_env_get_fd')
            import :: c_int, c_ptr, mdb_filehandle_t
            implicit none
            type(c_ptr),                    intent(in), value :: env
            integer(kind=mdb_filehandle_t), intent(out)       :: fd
            integer(kind=c_int)                               :: mdb_env_get_fd
        end function mdb_env_get_fd
    end interface
#endif
contains
    subroutine c_f_str_ptr(c_str, f_str, n)
        !! Copies a C string, passed as a C pointer, to a Fortran string.
        type(c_ptr),                   intent(in)           :: c_str
        character(len=:), allocatable, intent(out)          :: f_str
        integer(kind=c_size_t),        intent(in), optional :: n

        character(kind=c_char), pointer :: ptrs(:)
        integer(kind=c_size_t)          :: i, sz

        interface
            ! size_t strlen(const char *str)
            function c_strlen(str) bind(c, name='strlen')
                import :: c_ptr, c_size_t
                implicit none
                type(c_ptr), intent(in), value :: str
                integer(kind=c_size_t)         :: c_strlen
            end function c_strlen
        end interface

        copy_if: if (c_associated(c_str)) then
            if (present(n)) then
                sz = n
            else
                sz = c_strlen(c_str)
            end if

            if (sz < 0) exit copy_if
            call c_f_pointer(c_str, ptrs, [ sz ])
            allocate (character(len=sz) :: f_str)

            do i = 1, sz
                f_str(i:i) = ptrs(i)
            end do

            return
        end if copy_if

        if (.not. allocated(f_str)) f_str = ''
    end subroutine c_f_str_ptr

    function mdb_dbi_open(txn, name, flags, dbi) result(rc)
        type(c_ptr),      intent(inout) :: txn
        character(len=*), intent(in)    :: name
        integer,          intent(in)    :: flags
        integer,          intent(out)   :: dbi
        integer                         :: rc

        rc = mdb_dbi_open_(txn, trim(name) // c_null_char, flags, dbi)
    end function mdb_dbi_open

    subroutine mdb_env_close(env)
        type(c_ptr), intent(inout) :: env

        call mdb_env_close_(env)
        env = c_null_ptr
    end subroutine mdb_env_close

    function mdb_env_open(env, path, flags, mode) result(rc)
        type(c_ptr),      intent(inout) :: env
        character(len=*), intent(in)    :: path
        integer,          intent(in)    :: flags
        integer,          intent(in)    :: mode
        integer                         :: rc

        rc = mdb_env_open_(env, trim(path) // c_null_char, flags, int(mode, kind=mdb_mode_t))
    end function mdb_env_open

    function mdb_env_set_assert(env, func) result(rc)
        type(c_ptr), intent(in)    :: env
        procedure(mdb_assert_func) :: func
        integer                    :: rc

        rc = mdb_env_set_assert_(env, c_funloc(func))
    end function mdb_env_set_assert

    function mdb_reader_list(env, func, ctx) result(rc)
        type(c_ptr), intent(in) :: env
        procedure(mdb_msg_func) :: func
        type(c_ptr), intent(in) :: ctx
        integer                 :: rc

        rc = mdb_reader_list_(env, c_funloc(func), ctx)
    end function mdb_reader_list

    function mdb_set_compare(txn, dbi, cmp) result(rc)
        type(c_ptr),           intent(in) :: txn
        integer(kind=mdb_dbi), intent(in) :: dbi
        procedure(mdb_cmp_func)           :: cmp
        integer                           :: rc

        rc = mdb_set_compare_(txn, dbi, c_funloc(cmp))
    end function mdb_set_compare

    function mdb_set_dupsort(txn, dbi, cmp) result(rc)
        type(c_ptr),           intent(in) :: txn
        integer(kind=mdb_dbi), intent(in) :: dbi
        procedure(mdb_cmp_func)           :: cmp
        integer                           :: rc

        rc = mdb_set_dupsort_(txn, dbi, c_funloc(cmp))
    end function mdb_set_dupsort

    function mdb_set_relfunc(txn, dbi, rel) result(rc)
        type(c_ptr),           intent(in) :: txn
        integer(kind=mdb_dbi), intent(in) :: dbi
        procedure(mdb_rel_func)           :: rel
        integer                           :: rc

        rc = mdb_set_relfunc_(txn, dbi, c_funloc(rel))
    end function mdb_set_relfunc

    function mdb_strerror(err) result(str)
        integer, intent(in)           :: err
        character(len=:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = mdb_strerror_(err)
        call c_f_str_ptr(ptr, str)
    end function mdb_strerror

    subroutine mdb_txn_abort(txn)
        type(c_ptr), intent(inout) :: txn

        call mdb_txn_abort_(txn)
        txn = c_null_ptr
    end subroutine mdb_txn_abort

    function mdb_version(major, minor, patch) result(str)
        integer, intent(out)          :: major
        integer, intent(out)          :: minor
        integer, intent(out)          :: patch
        character(len=:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = mdb_version_(major, minor, patch)
        call c_f_str_ptr(ptr, str)
    end function mdb_version
end module lmdb
