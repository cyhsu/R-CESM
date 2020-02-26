module ocn_instance

    use seq_comm_mct, only: seq_comm_suffix,  &
                            seq_comm_inst,    &
                            seq_comm_name,    &
                            seq_comm_iamroot, &
                            seq_comm_namelen

    implicit none

    private

    public :: ocn_instance_init

    integer,                         public :: ocn_id
    integer,                         public :: inst_index
    character(len=seq_comm_namelen), public :: inst_name
    character(len=seq_comm_namelen), public :: inst_suffix
    logical,                         public :: inst_isroot

contains

    subroutine ocn_instance_init(in_ocn_id)

        integer, intent(in) :: in_ocn_id

        ocn_id      = in_ocn_id
        inst_name   = seq_comm_name   (ocn_id)
        inst_index  = seq_comm_inst   (ocn_id)
        inst_suffix = seq_comm_suffix (ocn_id)
        inst_isroot = seq_comm_iamroot(ocn_id)

    end subroutine ocn_instance_init

end module ocn_instance
