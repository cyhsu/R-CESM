module atm_instance

    use seq_comm_mct, only: seq_comm_suffix,  &
                            seq_comm_inst,    &
                            seq_comm_name,    &
                            seq_comm_iamroot, &
                            seq_comm_namelen

    implicit none

    private

    public :: atm_instance_init

    integer,                         public :: atm_id
    integer,                         public :: inst_index
    character(len=seq_comm_namelen), public :: inst_name
    character(len=seq_comm_namelen), public :: inst_suffix
    logical,                         public :: inst_isroot

contains

    subroutine atm_instance_init(in_atm_id)

        integer, intent(in) :: in_atm_id

        atm_id      = in_atm_id
        inst_name   = seq_comm_name   (atm_id)
        inst_index  = seq_comm_inst   (atm_id)
        inst_suffix = seq_comm_suffix (atm_id)
        inst_isroot = seq_comm_iamroot(atm_id)

    end subroutine atm_instance_init

end module atm_instance
