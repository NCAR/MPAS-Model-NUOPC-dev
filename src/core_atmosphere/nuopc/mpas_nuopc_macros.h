#define ESMF_ERR_RETURN(rc) if (ESMF_LogFoundError( \
        rcToCheck=rc, \
        msg=ESMF_LOGERR_PASSTHRU, \
        line=__LINE__, \
        file=__FILE__) \
    ) return

! Log and abort every PET. Use this instead of ESMF_ERR_RETURN wherever the
! other PETs are inside, or on their way into, a collective: returning locally
! leaves them blocked there forever and the job dies on walltime instead of on
! the error. ESMF_END_ABORT goes through MPI_Abort, so the whole job stops.
#define ESMF_ERR_ABORT(rc) if (ESMF_LogFoundError( \
        rcToCheck=rc, \
        msg=ESMF_LOGERR_PASSTHRU, \
        line=__LINE__, \
        file=__FILE__) \
    ) call ESMF_Finalize(endflag=ESMF_END_ABORT)
