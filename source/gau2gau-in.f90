program gau2gau_in

    !-------------------------------------------------
    ! This source is part of gau_Subs_gau interface
    !
    ! Description:
    ! ------------
    ! Reads input data file from Gaussian caller and
    ! generates a Gaussian input. 
    !
    ! The program expects to be called as:
    !
    !  gau2gau_in <input_data_fn> <route_section_fn> <outfile_label>
    !
    ! The input data file contains 
    ! (see https://gaussian.com/external/):
    !  #atoms  derivatives-requested  charge  spin
    !  atomic#  x  y  z  MM-charge (repeated for each atom)
    !
    !-------------------------------------------------

    implicit none

    real(8),parameter:: &
                           BOHRtoANGS= 5.2917720859D-1, &
                           UMAtoKG   = 1.66053873d-27,  &
                           UMAtoAU   = 1.82288839d3,    &
                           AUtoKG    = 9.10938291d-31,  &
                           BOHRtoM   = 5.291772083d-11, &
                           BOHRtoNM  = 5.291772083d-2,  &
                           AMStoM    = 1.d-10,          &
                           ANGStoM   = 1.d-10,          &
                           HARTtoJ   = 4.3597482d-18,   &
                           HtoKCALM  = 627.5095d0,      &
                           CALtoJ    = 4.184,           &
                           HtoeV     = 27.2114,         &
                           autown    = 2.1947463068d5    !From FCclasses Freq from AU to cm-1

    ! System vars
    integer :: Nat, IDer
    integer :: qtot,mult
    real(8),dimension(:),allocatable :: x,y,z,q
    integer,dimension(:),allocatable :: Zat
    ! IO
    character(len=200) :: input_file, route_file, out_label, out_gauinp
    integer :: I_INP=10, &
               I_ROU=11, &
               O_COM=20
    ! Reading route_file
    integer :: IOflag
    character(len=150) :: line
    character(len=10)  :: keyword,val
    character(len=10)  :: mem
    integer            :: nproc
    character(len=300) :: route_section
    
    ! Auxiliars
    character :: cnull
    ! Counters
    integer :: i


    ! Read command line args
    call getarg(1,input_file)
    call getarg(2,route_file)
    call getarg(3,out_label)


    ! Read input data file
    open(I_INP,file=input_file,status='old')
    read(I_INP,*) Nat, IDer, qtot, mult
    allocate(Zat(Nat),x(Nat),y(Nat),z(Nat),q(Nat))
    do i=1,Nat
        read(I_INP,*) Zat(i), x(i), y(i), z(i), q(i)
    enddo
    close(I_INP)


    ! Read route section file
    open(I_ROU,file=route_file,status='old')
    ! Read link 0
    do
        read(I_ROU,'(A)',iostat=IOflag) line
        if (IOflag /= 0) exit
        if (index(adjustl(line),'%') == 1) then
            ! Link0 stuff
            call split_line(line,'=',keyword,val)
            keyword=adjustl(keyword)
            if (keyword(2:6) == 'nproc') then
                read(val,*) nproc
            else if (keyword(2:4) == 'mem') then
                mem=adjustl(val)
            endif
        elseif (index(adjustl(line),'#') == 1) then
            !route section starts
            route_section=adjustl(line)
            exit
        endif
    enddo
    ! Read route section if it is in multiple lines (until reaching blank line or EOF)
    do
        read(I_ROU,'(A)',iostat=IOflag) line
        if (IOflag /= 0) exit
        if (len_trim(line) == 0) exit
        route_section=trim(adjustl(route_section))//trim(adjustl(line))
    enddo
    close(I_ROU)


    ! Write gaussian input job
    out_gauinp=trim(adjustl(out_label))//'.com'
    open(O_COM,file=out_gauinp,status='replace')
    write(O_COM,'(A,I0)') '%nproc = ', nproc
    write(O_COM,'(A)')    '%mem = '//trim(adjustl(mem))
    write(O_COM,'(A)')    '%chk = '//trim(adjustl(out_label))//'.chk'
    write(O_COM,'(A)')    ' '
    write(O_COM,'(A)')    trim(adjustl(route_section))
    write(O_COM,'(A)')    ' '
    write(O_COM,'(A)')    'Running: '//trim(adjustl(out_label))
    write(O_COM,'(A)')    ' '
    write(O_COM,'(2(2X,I0))') qtot, mult
    do i=1,Nat
        write(O_COM,'(I5,2X,3(F12.6,X))') &
                              Zat(i), x(i)*BOHRtoANGS, y(i)*BOHRtoANGS, z(i)*BOHRtoANGS
    enddo
    write(O_COM,'(A)')    ' '
    close(O_COM)

    ! Deallocate
    deallocate(x,y,z,q,Zat)

    stop


    contains

    subroutine split_line(line,splitter,line_a,line_b)

        !Split a line from a given marker. If it is not present, it does not
        !split the line (the whole is preserved in line_a

        character(len=*),intent(in):: line,splitter
        character(len=*),intent(out):: line_a,line_b

        !local
        integer :: i,j
        !Auxiliar helps when line(input) is also one 
        !of the outputs, line_a or line_b
        character(len=(len(line_a))) :: aux_line_a

        i=INDEX(line,splitter)
        if ( i == 0 ) then
            line_a=line
            line_b=""
            return
        endif
        j=len_trim(splitter)
        
        aux_line_a=line(1:i-1)
        line_b=line(i+j:)
        line_a=aux_line_a

        return

    end subroutine split_line

end program gau2gau_in
