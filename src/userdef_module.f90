module userdef_module
use amr_module
use amrray_module
use rtglobal_module
use constants_module
use dust_module
use lines_module
use stars_module
use quantum_module
use montecarlo_module
use namelist_module

!------------------------------------------------------------------------
! Here you can define your own variables, arrays etc. 
!------------------------------------------------------------------------

integer :: userdef_examplevariable      ! You can delete this, just an example
double precision, allocatable :: spintemp(:)
double precision :: spintmax=0.d0

contains

!------------------------------------------------------------------------
! This subroutine allows you to specify defaults values for your 
! variables.
!
! WARNING: In this subroutine you are not allowed to use write(stdo,*),
!          because the stdo is not yet set. Reason: The defaults are 
!          set before the command-line options are interpreted (so that
!          the defaults can be overwritten by command-line options),
!          but the stdo depends on whether the user calls RADMC-3D as
!          a child or not, which is given by the command-line options.
!------------------------------------------------------------------------
subroutine userdef_defaults()
  implicit none
  !
  userdef_examplevariable = 0           ! You can delete this, just an example
  !
end subroutine userdef_defaults

!------------------------------------------------------------------------
! Here you can interpret your own command-line options.
!------------------------------------------------------------------------
subroutine userdef_commandline(buffer,numarg,iarg,fromstdi,gotit)
  implicit none
  character*100 :: buffer
  integer :: iarg,numarg
  logical :: gotit,fromstdi
  !
  ! Below is just an example. You can delete or replace all of it.
  !
  if(buffer(1:16).eq.'examplecomlinopt') then
     userdef_examplevariable = 1
     gotit = .true.
  elseif(buffer(1:21).eq.'examplecomlinargument') then
     if(iarg.gt.numarg) then
        write(stdo,*) 'ERROR while reading command line options: cannot read sizeau.'
        write(stdo,*) '      Expecting 1 integer after examplecomlinargument.'
        stop
     endif
     call ggetarg(iarg,buffer,fromstdi)
     iarg = iarg+1
     read(buffer,*) userdef_examplevariable
     gotit = .true.
  else
     !
     ! NOTE: It is useful to keep this, as it will stop RADMC-3D if the user 
     !       accidently mistypes a command-line keyword instead of simply
     !       ignoring it (and thus possibly leaving the user convinced he/she
     !       did it right). 
     !
     write(stdo,*) 'ERROR: Could not recognize command line option ',trim(buffer)
     stop
  endif
  !
end subroutine userdef_commandline


!------------------------------------------------------------------------
! Here you can do some postprocessing after all command line options
! have been read. No example given here.
!------------------------------------------------------------------------
subroutine userdef_commandline_postprocessing()
  implicit none
end subroutine userdef_commandline_postprocessing


!------------------------------------------------------------------------
! Here you can parse your own keyword entries from the radmc3d.inp file.
!------------------------------------------------------------------------
subroutine userdef_parse_main_namelist()
  implicit none
  !
  ! And example how to include an integer keyword. Use
  ! parse_input_double and parse_input_word for reading doubleprecision
  ! variables or string variables. You can delete the below example.
  ! Note that the keyword name string should always have length 30,
  ! hence the many whitespaces.
  !
  call parse_input_integer('examplekeyword@               ',userdef_examplevariable)
  !
end subroutine userdef_parse_main_namelist


!------------------------------------------------------------------------
! Here you can do some post-processing after the radmc3d.inp namelist
! reading. 
!------------------------------------------------------------------------
subroutine userdef_main_namelist_postprocessing()
  implicit none
end subroutine userdef_main_namelist_postprocessing


!------------------------------------------------------------------------
! This is the place where you can define your own (base) grid setup,
! read your own frequency grid or set up your own stellar sources.
! No example given here, because it would interfere with basic operations.
!------------------------------------------------------------------------
subroutine userdef_prep_model()
  implicit none
end subroutine userdef_prep_model


!------------------------------------------------------------------------
! This is the place where you can set up your own model. By the time this
! subroutine is called the grid (at least a basic grid) is already set
! up. You can still modify the basic grid by adding more refinement, but
! to tell the AMR module to reserve space for more grid points you need
! to take matters into your own hand and create and init the base grid
! yourself in the userdef_prep_model() routine above. 
! No example given here, because it would interfere with basic operations.
!------------------------------------------------------------------------




!-------------------------------------------------------------------------
!                       READ SPINTEMPERATURE
!-------------------------------------------------------------------------


subroutine userdef_setup_model(action)

  implicit none
  integer :: action
  logical :: fex1,fex2,fex3
  double precision, allocatable :: data(:)
  double precision :: dummy
  integer(kind=8) :: iiformat,reclen,nn,kk
  integer :: icell,i,ierr,irec,index,idum,style,precis,reclenn
  !
  ! Action=0 means do nothing, action=1 means read if not yet read,
  ! action=2 means re-read.
  !
  !
  ! AF inserted these lines
  ! taken from the gas density
  !
  if(action.eq.0) then
     return
  elseif(action.eq.1) then
     if(allocated(spintemp)) return
  endif
  !
  !
  !
  ! AF commented out on 5th February 2016
  ! since the line broadening shall still be calculated with
  ! the gas temperature
  !
  !
  !if(action.eq.0) then
  !   return
  !elseif(action.eq.1) then
  !   if(allocated(gastemp)) then
  !      !
  !      ! The gas temperature is already determined. It might be
  !      ! that also the gastmax has been calculated from
  !      ! this, but we don't know for sure. So let's calculate
  !      ! it here to make sure gastmax is up-to-date. It
  !      ! can be important if the gas temperature is created
  !      ! using the userdef module.
  !      !
  !      ! Thanks, Rainer Rolffs, for pointing this out!
  !      ! 02.01.2011
  !      !
  !      gastmax = 0.d0
  !      do icell=1,nrcells
  !         index = cellindex(icell)
  !         if(gastemp(index).gt.gastmax) then
  !            gastmax = gastemp(index)
  !         endif
  !      enddo
  !      return
  !   endif
  !endif
  !
  !
  !
  ! AF commented out on 5th February 2016
  ! since also this is not needed
  ! the spin temperature shall not be equal to dust temperature
  !
  ! Default
  !
  precis = 8
  !
  ! If the user requests, then use one of the dust temperatures as gas
  ! temperature
  !
  ! if(tgas_eq_tdust.gt.0) then
  !   !
  !   ! Use dust temperature
  !   !
  !   ! First make sure the dust data are read
  !   ! if they are not already read yet
  !   !
  !   call read_dustdata(1)
  !   call read_dust_density(1)
  !   call read_dust_temperature(1)
  !   !
  !   ! Check something
  !   !
  !   if(tgas_eq_tdust.gt.dust_nr_species) then
  !      write(stdo,*) 'ERROR: tgas_eq_tdust is larger than dust_nr_species'
  !      stop
  !   endif
  !   !
  !   ! Allocate array
  !   !
  !   if(allocated(gastemp)) deallocate(gastemp)
  !   allocate(gastemp(1:nrcells),STAT=ierr)
  !   if(ierr.ne.0) then
  !      write(stdo,*) 'ERROR: Could not allocate gastemp array'
  !      stop
  !   endif
  !   write(stdo,*) 'Using dust temperature as gas temperature...'
  !   call flush(stdo)
  !   !
  !
  !   ! Now go and map the temperature
  !   !
  !   do icell=1,nrcells
  !      index = cellindex(icell)
  !      if((index.le.0).or.(index.gt.nrcellsmax)) then
  !         write(stdo,*) 'ERROR: Internal error while reading gas temperature'
  !         stop
  !      endif
  !      gastemp(index) = dusttemp(tgas_eq_tdust,index)
  !   enddo
  !else
     !
     ! Open temperature file
     !
     inquire(file='gas_spintemperature.inp',exist=fex1)
     inquire(file='gas_spintemperature.uinp',exist=fex2)
     inquire(file='gas_spintemperature.binp',exist=fex3)
     idum=0
     if(fex1) idum=idum+1
     if(fex2) idum=idum+1
     if(fex3) idum=idum+1
     if(idum.gt.1) then
        write(stdo,*) 'ERROR: Found more than one file gas_spintemperature.*inp'
        stop
     endif
     if(idum.eq.0) then
        write(stdo,*) 'ERROR: Could not find any gas_spintemperature.*inp file...'
        stop
     endif
     write(stdo,*) 'Reading spintemperature...'
     call flush(stdo)
     !
     ! Now read this temperature
     !
     if(fex1) then
        write(stdo,*) 'Fex1 format loop is called'
        !
        ! Formatted gas temperature file
        !
        style = 1
        open(unit=1,file='gas_spintemperature.inp',status='old')
        read(1,*) iiformat
        if(iiformat.ne.1) then
           write(stdo,*) 'ERROR: Format number of gas_spintemperature.inp is invalid/unknown.'
           stop
        endif
        read(1,*) nn
        write(stdo,*) 'and here nn is'
        write(stdo,*) nn
     elseif(fex2) then
        write(stdo,*) 'Fex2 format loop is called'
        !
        ! F77-style unformatted input of spintemperature
        !
        style = 2
        open(unit=1,file='gas_spintemperature.uinp',status='old',form='unformatted')
        read(1) iiformat,reclen
        if(iiformat.ne.1) then
           write(stdo,*) 'ERROR: Format number of gas_spintemperature.uinp is invalid/unknown.'
           stop
        endif
        reclenn=reclen
        read(1) nn
     else
        !
        ! Binary input: C-compliant unformatted streaming data
        !
        style = 3
        open(unit=1,file='gas_spintemperature.binp',status='old',access='stream')
        read(1) iiformat
        if(iiformat.ne.1) then
           write(stdo,*) 'ERROR: Format number of gas_spintemperature.binp is invalid/unknown.'
           stop
        endif
        read(1) nn
        precis = nn
        read(1) nn
     endif
     !
     ! Do some checks
     !
     if(nn.ne.nrcellsinp) then
        write(stdo,*) 'ERROR: gas_spintemperature.*inp does not have same number'
        write(stdo,*) '       of cells as the grid.'
        write(stdo,*) nn,nrcellsinp
        stop
     endif
     !
     ! Allocate array
     !
     if(allocated(spintemp)) deallocate(spintemp)
     allocate(spintemp(1:nrcells),STAT=ierr)
     if(ierr.ne.0) then
        write(stdo,*) 'ERROR: Could not allocate spintemp array'
        stop
     endif
     !
     ! Now read the spin temperature
     !
     call read_scalarfield(1,style,precis,nrcellsinp,1,1,1,1,1d-99,reclenn, &
                           scalar0=spintemp)
     !
     ! Close the file
     !
     close(1)
     !
  !endif !! commented out since the if and else statements above are no longer there
  !
  ! Compute the maximum temperature
  !
  spintmax = 0.d0
  do icell=1,nrcells
     index = cellindex(icell)
     if(spintemp(index).gt.spintmax) then
        spintmax = spintemp(index)
     endif
  enddo
  !
!end subroutine read_spintemperature
end subroutine userdef_setup_model


!------------------------------------------------------------------------
! If you want to do some calculation for the model after the main 
! calculation, you can do it here. Here you can also write stuff to
! file. 
!------------------------------------------------------------------------
subroutine userdef_dostuff()
  implicit none
end subroutine userdef_dostuff


!------------------------------------------------------------------------
! If you want to do design your own action (like 'mctherm' or 'image' but
! now designed by you entirely, and activated with 'radmc3d myaction')
! then here is your chance!
!------------------------------------------------------------------------
subroutine userdef_action()
  implicit none
end subroutine userdef_action


!------------------------------------------------------------------------
! If you want to use a Voigt line profile instead of a Gaussian line
! profile, you must initialize the variable lines_ray_lorentz_delta
! here.
! (Added by Thomas Peters 2011)
!------------------------------------------------------------------------
subroutine userdef_compute_lorentz_delta(ray_index)
  implicit none
  integer :: ray_index
end subroutine userdef_compute_lorentz_delta


!------------------------------------------------------------------------
! If you have a good idea how to calculate the level populations 
! of a molecule on-the-fly, you can do it here. But to activate it,
! you must put the line mode to -2.
! IMPORTANT NOTE: If you use the method of selecting a subset of the
!                 levels of a molecule, then you must be very careful
!                 in this subroutine to do it right. You must then use
!                 the "active_***" arrays and variables in the line
!                 module to figure out which levels are "active" and 
!                 which are not. 
!------------------------------------------------------------------------
subroutine userdef_compute_levelpop(ispec,nlevels,index,x,y,z, &
                                    numberdens,levelpop)
  implicit none
  integer :: ispec,nlevels,index
  double precision :: x,y,z,numberdens,levelpop(nlevels)
end subroutine userdef_compute_levelpop

!------------------------------------------------------------------------
! This routine lets you calculate the level populations entirely from
! scratch. Use lines_mode = -10 to use this routine.
!------------------------------------------------------------------------

subroutine userdef_general_compute_levelpop(ray_index, levelpop)
  implicit none
  integer :: ray_index
  double precision :: levelpop(1:lines_nrlevels_subset_max,1:lines_nr_species)
end subroutine userdef_general_compute_levelpop


!------------------------------------------------------------------------
! This subroutine allows you to specify exactly according to your own
! recipes/ideas the emissivity coefficient j_nu [erg/s/cm^3/Hz/ster]
! and extinction coefficient alpha_nu [1/cm] at the wavelengths given
! and at the location given. 
!
! ARGUMENTS:
!  index        The array index of the cell. This allows you to find
!               e.g. the gas density gasdens(index) or the gas
!               temperature gastemp(index) or any other quantity,
!               provided it is read in into the code. 
!  nrfreq       Nr of frequencies of the freq(:) array
!  freq(:)      Array of frequencies [Hz]
!  inu0,inu1    Starting/ending index: Calculate only src(inu0:inu1) and
!               alp(inu0:inu1) belonging to freq(inu0:inu1).
!
! RESULTS:
!  src(:)       Emissivity [erg/s/cm^3/Hz/ster]
!  alp(:)       Extinction [1/cm]
!
! Note: To activate this, you must set incl_userdef_srcalp = 1 in the
!       radmc3d.inp input file (in the code this is the logical 
!       rt_incl_userdef_srcalp from rtglobal_module.f90).
!
! Note: By the time RADMC-3D call this code, it has already computed
!       its own src(:) and alp(:). So just ADD your own values by e.g.
!       src(:) = src(:) + yourstuff and alp(:) = alp(:) + yourstuff.
!       In this way you won't delete the standard stuff. But if you
!       want to replace RADMC-3D's own stuff, you can also just write
!       src(:) = yourstuff and alp(:) = yourstuff. This is up to you.
!
! Note: Below you find some example, but commented-out, stuff.
!------------------------------------------------------------------------
subroutine userdef_srcalp(index,nrfreq,inu0,inu1,freq,src,alp)
  implicit none
  integer :: index,nrfreq,inu0,inu1
  double precision :: freq(1:nrfreq),src(1:nrfreq,1:4),alp(1:nrfreq)
  ! !
  ! ! If the index.lt.1 then we are not in a cell
  ! !
  ! if(index.lt.1) return
  ! ! 
  ! ! Replace src with dummy emission 
  ! !  
  ! src(:,1) = gasdens(index)
  ! alp(:) = 1d-40
  ! !
end subroutine userdef_srcalp


!------------------------------------------------------------------------
! Here you can write model setup arrays (the stuff you have set up here
! in the userdef_module.f90) to standard RADMC-3D-readable files.
! This will only be done if radmc3d receives the 'writemodel' command
! on the command line. 
!------------------------------------------------------------------------
subroutine userdef_writemodel()
  implicit none
  call write_grid_file()
  call write_dust_density()
end subroutine userdef_writemodel


!------------------------------------------------------------------------
! Reset some action flags for next command?
!------------------------------------------------------------------------
subroutine userdef_reset_flags()
  implicit none
end subroutine userdef_reset_flags

end module userdef_module

