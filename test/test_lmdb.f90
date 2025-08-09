! test_lmdb.f90
!
! Author:  Philipp Engel
! Licence: ISC
program main
    use, intrinsic :: iso_c_binding
    use :: lmdb
    implicit none (type, external)

    character(len=*), parameter :: ENV_NAME = 'test'

    type :: db_type
        type(c_ptr)                   :: env = c_null_ptr
        integer                       :: dbi = 0
        character(len=:), allocatable :: env_name
        character(len=:), allocatable :: db_name
    end type db_type

    character(len=:), allocatable :: value
    integer                       :: rc
    type(db_type)                 :: db

    call db_version()

    db_block: block
        print '(">> Trying to open database ...")'
        rc = db_open(db, env_name=ENV_NAME, db_name='fortran', map_size=10485760, max_dbs=1, max_readers=1)
        if (db_is_error(rc)) exit db_block

        print '(">> Inserting key, value ...")'
        rc = db_insert(db, key='test', value='Fortran')
        if (db_is_error(rc)) exit db_block

        print '(">> Reading key, value ...")'
        rc = db_get(db, key='test', value=value)
        if (db_is_error(rc)) exit db_block

        print '(">> Validating value ...")'
        if (value /= 'Fortran') then
            print '("Error: value mismatch")'
            exit db_block
        end if

        print '(">> Deleting key ...")'
        rc = db_delete(db, 'test')
    end block db_block

    if (db_is_error(rc)) print '("Error: ", a, " (", i0, ")")', mdb_strerror(rc), rc

    print '(">> Closing database ...")'
    call db_close(db)

    print '(">> Deleting database ...")'
    call db_purge(ENV_NAME)

    if (c_associated(db%env)) stop 'Error: MDB_env still associated (compiler bug)'
contains
    subroutine db_close(db)
        type(db_type), intent(inout) :: db

        if (.not. c_associated(db%env)) return
        if (db%dbi > 0) call mdb_dbi_close(db%env, db%dbi)
        call mdb_env_close(db%env)
    end subroutine db_close

    integer function db_delete(db, key) result(rc)
        type(db_type),            intent(inout) :: db
        character(len=*), target, intent(in)    :: key

        integer                :: dbi
        type(mdb_envinfo_type) :: info
        type(mdb_val_type)     :: k
        type(mdb_stat_type)    :: stat
        type(c_ptr)            :: txn

        k%mv_size = len(key, kind=c_size_t)
        k%mv_data = c_loc(key)

        print '("-- Getting environment information ...")'
        rc = mdb_env_info(db%env, info)
        if (db_is_error(rc)) return
        print '("-- Last transaction id: ", i0)', info%me_last_txnid

        print '("-- Creating transaction ...")'
        rc = mdb_txn_begin(db%env, c_null_ptr, 0, txn)
        if (db_is_error(rc)) return

        print '("-- Opening database ", a, " in environment ", a, " ...")', db%db_name, db%env_name
        rc = mdb_dbi_open(txn, db%db_name, 0, dbi)
        if (db_is_error(rc)) return

        print '("-- Deleting key ", a, " ...")', key
        rc = mdb_del(txn, db%dbi, k, c_null_ptr)

        if (.not. db_is_error(rc)) then
            print '("-- Committing transaction ...")'
            rc = mdb_txn_commit(txn)
            if (db_is_error(rc)) return
        else
            print '("-- Aborting transaction ...")'
            call mdb_txn_abort(txn)
            return
        end if

        rc = mdb_env_stat(db%env, stat)
        if (db_is_error(rc)) return
    end function db_delete

    pure elemental logical function db_is_error(rc)
        integer, intent(in) :: rc

        db_is_error = (rc /= MDB_SUCCESS)
    end function db_is_error

    integer function db_get(db, key, value) result(rc)
        type(db_type),                 intent(inout) :: db
        character(len=*), target,      intent(in)    :: key
        character(len=:), allocatable, intent(out)   :: value

        integer                :: dbi
        type(mdb_envinfo_type) :: info
        type(mdb_val_type)     :: k, v
        type(mdb_stat_type)    :: stat
        type(c_ptr)            :: txn

        k%mv_size = len(key, kind=c_size_t)
        k%mv_data = c_loc(key)

        db_block: block
            print '("-- Getting environment information ...")'
            rc = mdb_env_info(db%env, info)
            if (db_is_error(rc)) exit db_block
            print '("-- Last transaction id: ", i0)', info%me_last_txnid

            print '("-- Creating transaction ...")'
            rc = mdb_txn_begin(db%env, c_null_ptr, 0, txn)
            if (db_is_error(rc)) exit db_block

            print '("-- Opening database ", a, " in environment ", a, " ...")', db%db_name, db%env_name
            rc = mdb_dbi_open(txn, db%db_name, 0, dbi)
            if (db_is_error(rc)) exit db_block

            print '("-- Getting value of key ", a, " ...")', key
            rc = mdb_get(txn, db%dbi, k, v)

            if (.not. db_is_error(rc)) then
                call c_f_str_ptr(v%mv_data, value, v%mv_size)
                print '("-- Committing transaction ...")'
                rc = mdb_txn_commit(txn)
                if (db_is_error(rc)) exit db_block
            else
                print '("-- Aborting transaction ...")'
                call mdb_txn_abort(txn)
                exit db_block
            end if

            rc = mdb_env_stat(db%env, stat)
            if (db_is_error(rc)) exit db_block
        end block db_block

        if (.not. allocated(value)) value = ''
    end function db_get

    integer function db_insert(db, key, value) result(rc)
        type(db_type),            intent(inout) :: db
        character(len=*), target, intent(in)    :: key
        character(len=*), target, intent(in)    :: value

        integer                :: dbi
        type(mdb_envinfo_type) :: info
        type(mdb_val_type)     :: k, v
        type(mdb_stat_type)    :: stat
        type(c_ptr)            :: txn

        k%mv_size = len(key, kind=c_size_t)
        k%mv_data = c_loc(key)

        v%mv_size = len(value, kind=c_size_t)
        v%mv_data = c_loc(value)

        print '("-- Getting environment information ...")'
        rc = mdb_env_info(db%env, info)
        if (db_is_error(rc)) return
        print '("-- Last transaction id: ", i0)', info%me_last_txnid

        print '("-- Creating transaction ...")'
        rc = mdb_txn_begin(db%env, c_null_ptr, 0, txn)
        if (db_is_error(rc)) return

        print '("-- Opening database ", a, " in environment ", a, " ...")', db%db_name, db%env_name
        rc = mdb_dbi_open(txn, db%db_name, 0, dbi)
        if (db_is_error(rc)) return

        print '("-- Putting key, value into database ...")'
        rc = mdb_put(txn, db%dbi, k, v, MDB_NOOVERWRITE)

        if (.not. db_is_error(rc)) then
            print '("-- Key ", a, " inserted")', key
            print '("-- Committing transaction ...")'
            rc = mdb_txn_commit(txn)
            if (db_is_error(rc)) return
        else
            print '("-- Aborting transaction ...")'
            call mdb_txn_abort(txn)
            return
        end if

        rc = mdb_env_stat(db%env, stat)
        if (db_is_error(rc)) return
    end function db_insert

    integer function db_open(db, env_name, db_name, map_size, max_dbs, max_readers) result(rc)
        type(db_type),    intent(inout)        :: db
        character(len=*), intent(in)           :: env_name
        character(len=*), intent(in)           :: db_name
        integer,          intent(in), optional :: map_size
        integer,          intent(in), optional :: max_dbs
        integer,          intent(in), optional :: max_readers

        type(c_ptr) :: txn

        db%env_name = trim(env_name)
        db%db_name  = trim(db_name)

        print '("-- Creating environment ...")'
        rc = mdb_env_create(db%env)
        if (db_is_error(rc)) return

        if (present(map_size)) then
            print '("-- Setting map size ...")'
            rc = mdb_env_set_mapsize(db%env, int(map_size, kind=c_size_t))
            if (db_is_error(rc)) return
        end if

        if (present(max_dbs)) then
            print '("-- Setting max. number of named databases ...")'
            rc = mdb_env_set_maxdbs(db%env, max_dbs)
            if (db_is_error(rc)) return
        end if

        if (present(max_readers)) then
            print '("-- Setting max. number of readers ...")'
            rc = mdb_env_set_maxreaders(db%env, max_readers)
            if (db_is_error(rc)) return
        end if

        print '("-- Opening environment ", a, " ...")', db%env_name
        rc = mdb_env_open(db%env, db%env_name, ior(MDB_FIXEDMAP, MDB_NOSYNC), int(o'0664'))
        if (db_is_error(rc)) return

        print '("-- Creating transaction ...")'
        rc = mdb_txn_begin(db%env, c_null_ptr, 0, txn)
        if (db_is_error(rc)) return

        print '("-- Opening database ", a, " in environment ", a, " ...")', db%db_name, db%env_name
        rc = mdb_dbi_open(txn, db%db_name, MDB_CREATE, db%dbi)
        if (db_is_error(rc)) return

        print '("-- Committing transaction ...")'
        rc = mdb_txn_commit(txn)
        if (db_is_error(rc)) return
    end function db_open

    subroutine db_purge(env_name)
        character(len=*), intent(in) :: env_name

        call delete(trim(env_name) // '/data.mdb')
        call delete(trim(env_name) // '/lock.mdb')
    end subroutine db_purge

    subroutine db_version()
        character(len=:), allocatable :: version
        integer                       :: major, minor, patch

        version = mdb_version(major, minor, patch)
        print '(a, " (", 2(i0, "."), i0, ")")', version, major, minor, patch
    end subroutine db_version

    subroutine delete(path)
        character(len=*), intent(in) :: path

        integer :: file_unit, stat
        logical :: exists

        inquire (exist=exists, file=trim(path))
        if (.not. exists) return
        open (action='write', file=trim(path), iostat=stat, newunit=file_unit, status='old')
        close (file_unit, status='delete')
    end subroutine delete
end program main
