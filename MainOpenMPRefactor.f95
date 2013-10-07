!------------------------------------------------------------------------------
!        IST/MARETEC, Water Modelling Group, Mohid modelling system
!------------------------------------------------------------------------------
!
! TITLE         : Higher Order Functions
! DATE          : July 2013
! REVISION      : Ricardo Miranda
! DESCRIPTION   : Example program of Functional Core, Imperative LotkaVolterra according to
!                 Gary Bernhardt
!
!------------------------------------------------------------------------------
!
!This program is free software; you can redistribute it and/or
!modify it under the terms of the GNU General Public License
!version 2, as published by the Free Software Foundation.
!
!This program is distributed in the hope that it will be useful,
!but WITHOUT ANY WARRANTY; without even the implied warranty of
!MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!GNU General Public License for more details.
!
!You should have received a copy of the GNU General Public License
!along with this program; if not, write to the Free Software
!Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
!
!------------------------------------------------------------------------------

program MainOpenMPRefactor

    use ModuleopenMPRefactor

    implicit none

    type T_MainGeom
        real(8) :: DT, NbrSteps
    end type T_MainGeom

    call Main


    !Subroutines---------------------------------------------------------------

    !Constructor
!    public  :: ConstructMainGeom
!    private ::      ASkQuestions
!    private ::      StartGeom
!    private :: AllocateReplica

    !Selector

    !Modifier
!    public  :: Main
!    private ::      Loop
!    private ::      exampleFunction

    !Destructor
!    public  :: KillMainGeom

    !---------------------------------------------------------------------------

    contains

    !--------------------------------------------------------------------------

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !CONSTRUCTOR CONSTRUCTOR CONSTRUCTOR CONSTRUCTOR CONSTRUCTOR CONSTRUCTOR CONS

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    subroutine ConstructMainGeom (ObjMainGeom)

        !Arguments---------------------------------------------------------------
        type(T_MainGeom), pointer :: ObjMainGeom

        !------------------------------------------------------------------------

        call AllocateInstance (ObjMainGeom)
        call ASkQuestions     (ObjMainGeom)

    end subroutine ConstructMainGeom

    !--------------------------------------------------------------------------

    subroutine ASkQuestions (ObjMainGeom)

        !Arguments---------------------------------------------------------------
        type(T_MainGeom), pointer :: ObjMainGeom

        !Local-------------------------------------------------------------------

        !------------------------------------------------------------------------

        print*, "What is the time step?"
        read*,  ObjMainGeom%DT

        print*, "How many time steps to compute?"
        read*,  ObjMainGeom%NbrSteps

    end subroutine ASkQuestions

    !--------------------------------------------------------------------------

    function StartGeom ()

        !Local-------------------------------------------------------------------
        integer :: IMIN, IMAX, ILB, IUB
        integer :: JMIN, JMAX, JLB, JUB

        !Return----------------------------------------------------------------
        type (T_Arrays), pointer  :: StartGeom

        !Local-----------------------------------------------------------------
        type (T_Arrays), pointer  :: NewObjGeom

        !----------------------------------------------------------------------

        print*, "IMIN?"
        read*,   IMIN

        print*, "IMAX?"
        read*,   IMAX

        print*, "ILB?"
        read*,   ILB

        print*, "IUB?"
        read*,   IUB

        print*, "JMIN?"
        read*,   JMIN

        print*, "JMAX?"
        read*,   JMAX

        print*, "JLB?"
        read*,   JLB

        print*, "JUB?"
        read*,   JUB

        NewObjGeom => ConstructGeom (IMIN, IMAX, ILB, IUB,                       &
                                   JMIN, JMAX, JLB, JUB)

        StartGeom  => NewObjGeom
    end function StartGeom

    !--------------------------------------------------------------------------

    pure subroutine AllocateInstance (ObjMainGeom)
        !Arguments-------------------------------------------------------------
        type(T_MainGeom), pointer :: ObjMainGeom

        !----------------------------------------------------------------------

        allocate (ObjMainGeom)

    end subroutine AllocateInstance

    !--------------------------------------------------------------------------

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !MODIFIER MODIFIER MODIFIER MODIFIER MODIFIER MODIFIER MODIFIER MODIFIER MODI

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !--------------------------------------------------------------------------

    subroutine Main

        !Local-----------------------------------------------------------------
        type(T_MainGeom), pointer :: ObjMainGeom
        type(T_Arrays ), pointer :: ObjGeom

        !----------------------------------------------------------------------

        call ConstructMainGeom (ObjMainGeom)

        ObjGeom => StartGeom ()
        call Loop          (ObjMainGeom, ObjGeom, ObjMainGeom%NbrSteps)

    end subroutine Main

    !--------------------------------------------------------------------------

    recursive subroutine Loop (ObjMainGeom, ObjGeom, NbrSteps)

        !Arguments-------------------------------------------------------------
        type(T_MainGeom), pointer :: ObjMainGeom
        type(T_Arrays  ), pointer :: ObjGeom
        real(8), intent(IN)       :: NbrSteps

        !Local-----------------------------------------------------------------
        type(T_Arrays ), pointer :: NewObjGeom01, NewObjGeom02, NewObjGeom03

        !----------------------------------------------------------------------

cd1 :   if (NbrSteps .LE. 0.0) then
            print*, "Simulation terminated successfully."

            call killMainGeom (ObjMainGeom, ObjGeom)

        else   cd1
            NewObjGeom01 => CalcResult01(ObjGeom,      FaceWaterColumn = 3)
            call GeomGarbageCollector   (ObjGeom)
            print*, NbrSteps, GetAreaV  (NewObjGeom01)

            NewObjGeom02 => CalcResult02(NewObjGeom01, FaceWaterColumn = 3)
            call GeomGarbageCollector   (NewObjGeom01)
            print*, NbrSteps, GetAreaV  (NewObjGeom02)

            NewObjGeom03 => CalcResult03(NewObjGeom02, FaceWaterColumn = 3)
            call GeomGarbageCollector   (NewObjGeom02)
            print*, NbrSteps, GetAreaV  (NewObjGeom03)

            call Loop (ObjMainGeom, NewObjGeom03, NbrSteps - ObjMainGeom%DT)
        end if cd1

    end subroutine Loop

    !--------------------------------------------------------------------------


    !--------------------------------------------------------------------------

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



    pure subroutine KillMainGeom (ObjMainGeom, ObjGeom)
        !Arguments-------------------------------------------------------------
        type(T_MainGeom), pointer :: ObjMainGeom
        type(T_Arrays ), pointer :: ObjGeom

        !----------------------------------------------------------------------

        call KillGeom (ObjGeom)

        deallocate   (ObjMainGeom)

    end subroutine KillMainGeom

    !--------------------------------------------------------------------------

end program MainOpenMPRefactor
