!------------------------------------------------------------------------------
!        IST/MARETEC, Water Modelling Group, Mohid modelling system
!------------------------------------------------------------------------------
!
! TITLE         : Higher Order Functions
! DATE          : July 2013
! REVISION      : Ricardo Miranda
! DESCRIPTION   : Example program of Functional Core, Imperative Shell according to
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

Module ModuleopenMPRefactor

    implicit none

    !Types---------------------------------------------------------------------

    public :: T_Arrays
    type      T_Arrays
        private
        real(8), dimension(:, :),  pointer :: AreaV
        !Computed every time step

        real(8), dimension(:, :),  pointer :: myWaterLevel
        real(8), dimension(:, :),  pointer :: BuildingsHeight
        real(8), dimension(:, :),  pointer :: Topography

        integer, dimension(:, :),  pointer :: BasinPoints

        integer :: BasinPoint = 1
        integer :: WCMaxBottom_ = 2
        integer :: WCAverageBottom_ = 3

        integer, pointer :: IMIN, IMAX, ILB, IUB
        integer, pointer :: JMIN, JMAX, JLB, JUB
    end type  T_Arrays

    !--------------------------------------------------------------------------

    private

    integer :: NULL_INT  =-99999
    real(8) :: NULL_REAL =-99999.99

    !Subroutines---------------------------------------------------------------

    !Constructor
    public  :: ConstructGeom
    private ::      AllocateInstance
    private ::      InitializeValues
    private :: AllocateReplica

    !Selector
    public  :: GetmyWaterLevel
    public  :: GetBuildingsHeight
    public  :: GetTopography
    public  :: GetBasinPoints
    public  :: GetAreaV
    public  :: GetIMIN
    public  :: GetIMAX
    public  :: GetILB
    public  :: GetIUB
    public  :: GetJMIN
    public  :: GetJMAX
    public  :: GetJLB
    public  :: GetJUB

    !Modifier
    public  :: CalcResult01
    public  :: CalcResult02
    private ::      WCA
    private ::          WaterColumn     !Bottom
    public  :: CalcResult03
    public  ::      CalcResultHO

    !Destructor
    public  :: KillGeom
    private ::      DeAllocateInstance
    public  :: GeomGarbageCollector

    !Interfaces----------------------------------------------------------------

    !--------------------------------------------------------------------------

    contains


    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !CONSTRUCTOR CONSTRUCTOR CONSTRUCTOR CONSTRUCTOR CONSTRUCTOR CONSTRUCTOR CONS

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    pure function ConstructGeom(IMIN, IMAX, ILB, IUB,                           &
                               JMIN, JMAX, JLB, JUB)

        !Arguments---------------------------------------------------------------
        integer, intent(IN) :: IMIN, IMAX, ILB, IUB
        integer, intent(IN) :: JMIN, JMAX, JLB, JUB

        !Return------------------------------------------------------------------
        type (T_Arrays), pointer :: ConstructGeom

        !Local-------------------------------------------------------------------
        type (T_Arrays), pointer :: NewObjGeom

        !------------------------------------------------------------------------

        call AllocateInstance(NewObjGeom,                                       &
                              ILB, IUB,                                         &
                              JLB, JUB)

        call InitializeValues(NewObjGeom,                                       &
                              IMIN, IMAX, ILB, IUB,                             &
                              JMIN, JMAX, JLB, JUB)

        ConstructGeom => NewObjGeom

        !----------------------------------------------------------------------

    end function ConstructGeom

    !--------------------------------------------------------------------------

    pure subroutine AllocateInstance(NewObjGeom,                                &
                                     ILB, IUB,                                  &
                                     JLB, JUB)

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: NewObjGeom

        integer, intent(IN) :: ILB, IUB
        integer, intent(IN) :: JLB, JUB

        !Local-----------------------------------------------------------------

        !Allocates new instance
        allocate (NewObjGeom)
        allocate (NewObjGeom%myWaterLevel    (ILB:IUB,                          &
                                              JLB:JUB))

        allocate (NewObjGeom%BuildingsHeight(ILB:IUB,                           &
                                             JLB:JUB))

        allocate (NewObjGeom%Topography     (ILB:IUB,                           &
                                             JLB:JUB))

        allocate (NewObjGeom%BasinPoints    (ILB:IUB,                           &
                                             JLB:JUB))

        allocate (NewObjGeom%AreaV          (ILB:IUB,                           &
                                             JLB:JUB))

        allocate (NewObjGeom%IMIN)
        allocate (NewObjGeom%IMAX)
        allocate (NewObjGeom%ILB )
        allocate (NewObjGeom%IUB )
        allocate (NewObjGeom%JMIN)
        allocate (NewObjGeom%JMAX)
        allocate (NewObjGeom%JLB )
        allocate (NewObjGeom%JUB )

    end subroutine AllocateInstance

    !--------------------------------------------------------------------------

    pure subroutine InitializeValues(NewObjGeom,                                &
                                     IMIN, IMAX, ILB, IUB,                      &
                                     JMIN, JMAX, JLB, JUB)

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: NewObjGeom

        integer, intent(IN) :: IMIN, IMAX, ILB, IUB
        integer, intent(IN) :: JMIN, JMAX, JLB, JUB

        !Local-----------------------------------------------------------------
        integer :: I, J

        !----------------------------------------------------------------------

        NewObjGeom%myWaterLevel     = NULL_REAL
        NewObjGeom%BuildingsHeight  = NULL_REAL
        NewObjGeom%Topography       = NULL_REAL
        NewObjGeom%BasinPoints      = NULL_INT
        NewObjGeom%AreaV            = NULL_REAL

do1:    DO I = IMIN, IMAX
do2:    DO J = JMIN, JMAX
            NewObjGeom%myWaterLevel      (I, J) = I * 2.1
            NewObjGeom%BuildingsHeight   (I, J) = J * 3.4
            NewObjGeom%Topography        (I, J) = J * 3.4
            NewObjGeom%BasinPoints       (I, J) = 1
        ENDDO do2
        ENDDO do1

        NewObjGeom%IMIN = IMIN
        NewObjGeom%IMAX = IMAX
        NewObjGeom%ILB  = ILB
        NewObjGeom%IUB  = IUB
        NewObjGeom%JMIN = JMIN
        NewObjGeom%JMAX = JMAX
        NewObjGeom%JLB  = JLB
        NewObjGeom%JUB  = JUB

    end subroutine InitializeValues

    !--------------------------------------------------------------------------

    function AllocateReplica(ObjGeom)

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer                               :: ObjGeom

        !Return----------------------------------------------------------------
        type (T_Arrays), pointer                               :: AllocateReplica

        !Local-----------------------------------------------------------------
        type (T_Arrays), pointer                               :: NewObjGeom

        !-----------------------------------------------------------------------

        !Allocates new values
        allocate (NewObjGeom)

        NewObjGeom%myWaterLevel     => ObjGeom%myWaterLevel
        NewObjGeom%BuildingsHeight  => ObjGeom%BuildingsHeight
        NewObjGeom%Topography       => ObjGeom%Topography
        NewObjGeom%BasinPoints      => ObjGeom%BasinPoints
        NewObjGeom%IMIN   => ObjGeom%IMIN
        NewObjGeom%IMAX   => ObjGeom%IMAX
        NewObjGeom%ILB    => ObjGeom%ILB
        NewObjGeom%IUB    => ObjGeom%IUB
        NewObjGeom%JMIN   => ObjGeom%JMIN
        NewObjGeom%JMAX   => ObjGeom%JMAX
        NewObjGeom%JLB    => ObjGeom%JLB
        NewObjGeom%JUB    => ObjGeom%JUB

        AllocateReplica  => NewObjGeom

    end function AllocateReplica

    !--------------------------------------------------------------------------


    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !SELECTOR SELECTOR SELECTOR SELECTOR SELECTOR SELECTOR SELECTOR SELECTOR SE

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



    !--------------------------------------------------------------------------

    function GetAreaV (ObjGeom)

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: ObjGeom

        !Return----------------------------------------------------------------
       real(8), dimension(:, :),  pointer :: GetAreaV

        !----------------------------------------------------------------------

        GetAreaV => ObjGeom%AreaV

    end function GetAreaV

    !--------------------------------------------------------------------------

    function GetmyWaterLevel (ObjGeom)

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: ObjGeom

        !Return----------------------------------------------------------------
       real(8), dimension(:, :),  pointer :: GetmyWaterLevel

        !----------------------------------------------------------------------

        GetmyWaterLevel => ObjGeom%myWaterLevel

    end function GetmyWaterLevel

    !--------------------------------------------------------------------------

    function GetBuildingsHeight (ObjGeom)

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: ObjGeom

        !Return----------------------------------------------------------------
        real(8), dimension(:, :),  pointer :: GetBuildingsHeight

        !----------------------------------------------------------------------

        GetBuildingsHeight => ObjGeom%BuildingsHeight

    end function GetBuildingsHeight

    !--------------------------------------------------------------------------

    function GetTopography (ObjGeom)

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: ObjGeom

        !Return----------------------------------------------------------------
        real(8), dimension(:, :),  pointer :: GetTopography

        !----------------------------------------------------------------------

        GetTopography => ObjGeom%Topography

    end function GetTopography

    !--------------------------------------------------------------------------

    function GetBasinPoints (ObjGeom)

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: ObjGeom

        !Return----------------------------------------------------------------
        integer, dimension(:, :),  pointer :: GetBasinPoints

        !----------------------------------------------------------------------

        GetBasinPoints => ObjGeom%BasinPoints

    end function GetBasinPoints

    !--------------------------------------------------------------------------

    pure function GetIMAX (ObjGeom)

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: ObjGeom

        !Return----------------------------------------------------------------
        integer :: GetIMAX

        !----------------------------------------------------------------------

        GetIMAX = ObjGeom%IMAX

    end function GetIMAX

    !--------------------------------------------------------------------------

    pure function GetIMIN (ObjGeom)

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: ObjGeom

        !Return----------------------------------------------------------------
        integer :: GetIMIN

        !----------------------------------------------------------------------

        GetIMIN = ObjGeom%IMIN

    end function GetIMIN

    !--------------------------------------------------------------------------

    pure function GetILB (ObjGeom)

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: ObjGeom

        !Return----------------------------------------------------------------
        integer :: GetILB

        !----------------------------------------------------------------------

        GetILB = ObjGeom%ILB

    end function GetILB

    !--------------------------------------------------------------------------

    pure function GetIUB (ObjGeom)

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: ObjGeom

        !Return----------------------------------------------------------------
        integer :: GetIUB

        !----------------------------------------------------------------------

        GetIUB = ObjGeom%IUB

    end function GetIUB

    !--------------------------------------------------------------------------

    pure function GetJMIN (ObjGeom)

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: ObjGeom

        !Return----------------------------------------------------------------
        integer :: GetJMIN

        !----------------------------------------------------------------------

        GetJMIN = ObjGeom%JMIN

    end function GetJMIN

    !--------------------------------------------------------------------------

    pure function GetJMAX (ObjGeom)

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: ObjGeom

        !Return----------------------------------------------------------------
        integer :: GetJMAX

        !----------------------------------------------------------------------

        GetJMAX = ObjGeom%JMAX

    end function GetJMAX

    !--------------------------------------------------------------------------

    pure function GetJLB (ObjGeom)

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: ObjGeom

        !Return----------------------------------------------------------------
        integer :: GetJLB

        !----------------------------------------------------------------------

        GetJLB = ObjGeom%JLB

    end function GetJLB

    !--------------------------------------------------------------------------

    pure function GetJUB (ObjGeom)

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: ObjGeom

        !Return----------------------------------------------------------------
        integer :: GetJUB

        !----------------------------------------------------------------------

        GetJUB = ObjGeom%JUB

    end function GetJUB

    !--------------------------------------------------------------------------

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !MODIFIER MODIFIER MODIFIER MODIFIER MODIFIER MODIFIER MODIFIER MODIFIER MODI

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    function CalcResult01(ObjGeom, FaceWaterColumn)

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: ObjGeom
        integer, intent(IN)      :: FaceWaterColumn

        !Return------------------------------------------------------------------
        type (T_Arrays), pointer :: CalcResult01

        !Local-------------------------------------------------------------------
        type (T_Arrays), pointer :: NewObjGeom
        real(8), dimension(:, :), pointer :: AreaV
        real(8), dimension(:, :), pointer :: WCL, WCR, WCA, Bottom
        integer :: I, J

        !----------------------------------------------------------------------

        NewObjGeom          => AllocateReplica(ObjGeom)

        allocate (AreaV (ObjGeom%ILB:ObjGeom%IUB,                               &
                         ObjGeom%JLB:ObjGeom%JUB))

        allocate (WCL   (ObjGeom%ILB:ObjGeom%IUB,                               &
                         ObjGeom%JLB:ObjGeom%JUB))
        allocate (WCR   (ObjGeom%ILB:ObjGeom%IUB,                               &
                         ObjGeom%JLB:ObjGeom%JUB))
        allocate (WCA   (ObjGeom%ILB:ObjGeom%IUB,                               &
                         ObjGeom%JLB:ObjGeom%JUB))
        allocate (Bottom(ObjGeom%ILB:ObjGeom%IUB,                               &
                         ObjGeom%JLB:ObjGeom%JUB))


!$OMP PARALLEL
!$OMP DO
do1 :   DO I = ObjGeom%IMIN, ObjGeom%IMAX
do2 :   DO J = ObjGeom%JMIN, ObjGeom%JMAX
if1 :       if (ObjGeom%BasinPoints(i, j-1) == ObjGeom%BasinPoint .AND.         &
                ObjGeom%BasinPoints(i, j  ) == ObjGeom%BasinPoint) then
if2 :           if (FaceWaterColumn == ObjGeom%WCMaxBottom_) then
                    !Maximum Bottom Level
                    Bottom(I, J) = max(ObjGeom%Topography(i, j-1), ObjGeom%Topography(i, j))
                elseif (FaceWaterColumn == ObjGeom%WCAverageBottom_) then if2
                    !Average Bottom Level
                    Bottom(I, J) = (ObjGeom%Topography(i,j) + ObjGeom%Topography(i,j-1)) / 2.0
                endif if2

                !Water Column Left (above MaxBottom)
                WCL(I, J)       = max(ObjGeom%myWaterLevel(i, j-1) + ObjGeom%BuildingsHeight(i, j-1) - Bottom(I, J), dble(0.0))

                !Water Column Right (above MaxBottom)
                WCR(I, J)       = max(ObjGeom%myWaterLevel(i, j  ) + ObjGeom%BuildingsHeight(i, j)   - Bottom(I, J), dble(0.0))


                !In the case of kinematic wave, always consider the "upstream" area, otherwise the average above "max bottom"
if3 :           if (FaceWaterColumn == ObjGeom%WCMaxBottom_) then
if4 :               if (FaceWaterColumn == ObjGeom%WCMaxBottom_) then
                        WCA(I, J) = WCL(I, J)
                    else if4
                        WCA(I, J) = WCR(I, J)
                    endif if4
                else if3
                    !Average Water Column
                    !WCA = (WCL + WCR) / 2.0
if5 :               if (FaceWaterColumn == ObjGeom%WCMaxBottom_ .AND.           &
                        FaceWaterColumn == ObjGeom%WCMaxBottom_) then
                        WCA(I, J) = WCL(I, J)
                    else if5
                        WCA(I, J) = WCR(I, J)
                    endif if5
                endif if3

                !Area  = Water Column * Side lenght of cell
                AreaV(i, j) = WCA(I, J) * ObjGeom%myWaterLevel(i, j-1)
            end if if1
        ENDDO do2
        ENDDO do1
!$OMP END DO NOWAIT
!$OMP END PARALLEL

        deallocate(WCL)
        deallocate(WCR)
        deallocate(WCA)
        deallocate(Bottom)

        NewObjGeom%AreaV => AreaV
        CalcResult01     => NewObjGeom

    end function CalcResult01

    !---------------------------------------------------------------------------

    function CalcResult02(ObjGeom, FaceWaterColumn)

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: ObjGeom
        integer, intent(IN)      :: FaceWaterColumn

        !Return------------------------------------------------------------------
        type (T_Arrays), pointer :: CalcResult02

        !Local-------------------------------------------------------------------
        type (T_Arrays), pointer :: NewObjGeom
        real(8), dimension(:, :), pointer :: AreaV
        integer :: I, J

        !----------------------------------------------------------------------

        NewObjGeom          => AllocateReplica(ObjGeom)

        allocate (AreaV (ObjGeom%ILB:ObjGeom%IUB,                               &
                         ObjGeom%JLB:ObjGeom%JUB))

!$OMP PARALLEL
!$OMP DO
do1 :   DO I = ObjGeom%IMIN, ObjGeom%IMAX
do2 :   DO J = ObjGeom%JMIN, ObjGeom%JMAX
if1 :       if (ObjGeom%BasinPoints(i, j-1) == ObjGeom%BasinPoint .AND.         &
                ObjGeom%BasinPoints(i, j  ) == ObjGeom%BasinPoint) then
                !Area  = Water Column * Side lenght of cell
                AreaV(i, j) = WCA(ObjGeom, FaceWaterColumn, I, J) * ObjGeom%myWaterLevel(i, j-1)
            end if if1
        ENDDO do2
        ENDDO do1
!$OMP END DO NOWAIT
!$OMP END PARALLEL

        NewObjGeom%AreaV => AreaV
        CalcResult02     => NewObjGeom

    end function CalcResult02

    !--------------------------------------------------------------------------

     function WCA(ObjGeom,                                                  &
                  FaceWaterColumn,                                          &
                  I, J)

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: ObjGeom
        integer, intent(IN)      :: FaceWaterColumn
        integer, intent(IN)      :: I, J

        !Return------------------------------------------------------------------
        real(8) :: WCA

        !Local-----------------------------------------------------------------

        real(8) :: WCA_

        !----------------------------------------------------------------------

        !In the case of kinematic wave, always consider the "upstream" area, otherwise the average above "max bottom"
if3 :   if (FaceWaterColumn == ObjGeom%WCMaxBottom_) then
if4 :       if (FaceWaterColumn == ObjGeom%WCMaxBottom_) then
                !Water Column Left (above MaxBottom)
                WCA_ = WaterColumn(ObjGeom, FaceWaterColumn, I, J, ObjGeom%myWaterLevel(I, J-1), ObjGeom%BuildingsHeight(I, J-1))
            else if4
                !Water Column Right (above MaxBottom)
                WCA_ = WaterColumn(ObjGeom, FaceWaterColumn, I, J, ObjGeom%myWaterLevel(I, J  ), ObjGeom%BuildingsHeight(I, J  ))
            endif if4
        else if3
            !Average Water Column
            !WCA = (WCL + WCR) / 2.0
if5 :   if (FaceWaterColumn == ObjGeom%WCMaxBottom_ .AND.           &
            FaceWaterColumn == ObjGeom%WCMaxBottom_) then
                !Water Column Left (above MaxBottom)
                WCA_ = WaterColumn(ObjGeom, FaceWaterColumn, I, J, ObjGeom%myWaterLevel(I, J-1), ObjGeom%BuildingsHeight(I, J-1))
            else if5
                !Water Column Right (above MaxBottom)
                WCA_ = WaterColumn(ObjGeom, FaceWaterColumn, I, J, ObjGeom%myWaterLevel(I, J  ), ObjGeom%BuildingsHeight(I, J  ))
            endif if5
        endif if3

        WCA = WCA_

        !-----------------------------------------------------------------------

    end function WCA

    !---------------------------------------------------------------------------

    function WaterColumn(ObjGeom,                                                  &
                              FaceWaterColumn,                                          &
                              I, J,                                                     &
                              myWaterLevel,                                             &
                              BuildingsHeight)
        !Arguments--------------------------------------------------------------
        type (T_Arrays), pointer :: ObjGeom
        integer, intent(IN)      :: FaceWaterColumn
        integer, intent(IN)      :: I, J
        real(8), intent(IN)      :: myWaterLevel, BuildingsHeight

        !Return------------------------------------------------------------------
        real(8) :: WaterColumn

        !-----------------------------------------------------------------------

        !Water Column Left (above MaxBottom)
        WaterColumn = max(myWaterLevel + BuildingsHeight - Bottom(), dble(0.0))

        !-----------------------------------------------------------------------

        contains

        !--------------------------------------------------------------------------

        pure function Bottom()

            !Arguments-------------------------------------------------------------

            !Return------------------------------------------------------------------
            real(8) :: Bottom

            !Local-----------------------------------------------------------------

            real(8) :: Bottom_

            !----------------------------------------------------------------------

if2 :       if (FaceWaterColumn == ObjGeom%WCMaxBottom_) then
                !Maximum Bottom Level
                Bottom_ = max(ObjGeom%Topography(i, j-1), ObjGeom%Topography(i, j))
            elseif (FaceWaterColumn == ObjGeom%WCAverageBottom_) then if2
                !Average Bottom Level
                Bottom_ = (ObjGeom%Topography(i,j) + ObjGeom%Topography(i,j-1)) / 2.0
            endif if2

            Bottom = Bottom_

            !-----------------------------------------------------------------------

        end function Bottom

        !-----------------------------------------------------------------------

    end function WaterColumn

    !---------------------------------------------------------------------------

    function CalcResult03(ObjGeom, FaceWaterColumn)

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: ObjGeom
        integer, intent(IN)      :: FaceWaterColumn

        !Return------------------------------------------------------------------
        type (T_Arrays), pointer :: CalcResult03

        !Local-------------------------------------------------------------------
        type (T_Arrays), pointer :: NewObjGeom

        !----------------------------------------------------------------------

        NewObjGeom       => AllocateReplica(ObjGeom)


        NewObjGeom%AreaV => CalcResultHO   (ObjGeom,                            &
                                            FaceWaterColumn,                    &
                                            WCA)

        CalcResult03     => NewObjGeom

        !-----------------------------------------------------------------------

    end function CalcResult03

    !--------------------------------------------------------------------------

    function CalcResultHO(ObjGeom, FaceWaterColumn, f)

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: ObjGeom
        integer, intent(IN)      :: FaceWaterColumn
        real(8) :: f

        !Return------------------------------------------------------------------
        real(8), dimension(:, :), pointer :: CalcResultHO

        !Local-------------------------------------------------------------------
        real(8), dimension(:, :), pointer :: AreaV
        integer :: I, J

        !----------------------------------------------------------------------

       allocate (AreaV (ObjGeom%ILB:ObjGeom%IUB,                               &
                         ObjGeom%JLB:ObjGeom%JUB))

!$OMP PARALLEL
!$OMP DO
do1 :   DO I = ObjGeom%IMIN, ObjGeom%IMAX
do2 :   DO J = ObjGeom%JMIN, ObjGeom%JMAX
if1 :       if (ObjGeom%BasinPoints(i, j-1) == ObjGeom%BasinPoint .AND.         &
                ObjGeom%BasinPoints(i, j  ) == ObjGeom%BasinPoint) then
                !Area  = Water Column * Side lenght of cell
                AreaV(i, j) = f(ObjGeom, FaceWaterColumn, I, J) * ObjGeom%myWaterLevel(i, j-1)
            end if if1
        ENDDO do2
        ENDDO do1
!$OMP END DO NOWAIT
!$OMP END PARALLEL

        CalcResultHO => AreaV

        !-----------------------------------------------------------------------

    end function CalcResultHO


    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



    pure subroutine KillGeom(ObjGeom)

        !Arguments---------------------------------------------------------------
        type (T_Arrays), pointer :: ObjGeom

        !Return------------------------------------------------------------------

        !External----------------------------------------------------------------

        !Local-------------------------------------------------------------------

        !------------------------------------------------------------------------

        !Deallocates Instance
        call DeallocateInstance (ObjGeom)

        !------------------------------------------------------------------------

    end subroutine KillGeom


    !------------------------------------------------------------------------


    pure subroutine DeallocateInstance (ObjGeom)

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: ObjGeom

        !Local-----------------------------------------------------------------

        !------------------------------------------------------------------------

        !Deallocates instance
        deallocate (ObjGeom%IMIN)
        deallocate (ObjGeom%IMAX)
        deallocate (ObjGeom%ILB )
        deallocate (ObjGeom%IUB )
        deallocate (ObjGeom%JMIN)
        deallocate (ObjGeom%JMAX)
        deallocate (ObjGeom%JLB )
        deallocate (ObjGeom%JUB )

        deallocate (ObjGeom%AreaV)
        deallocate (ObjGeom%myWaterLevel)
        deallocate (ObjGeom%BuildingsHeight)
        deallocate (ObjGeom%Topography)
        deallocate (ObjGeom%BasinPoints)
        deallocate (ObjGeom)

    end subroutine DeallocateInstance

    !--------------------------------------------------------------------------

    pure subroutine GeomGarbageCollector (ObjGeom)

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: ObjGeom

        !Local-----------------------------------------------------------------

        !------------------------------------------------------------------------

        !Partially deallocates instance
        deallocate (ObjGeom%AreaV)
        deallocate (ObjGeom)

        !------------------------------------------------------------------------

    end subroutine GeomGarbageCollector

    !--------------------------------------------------------------------------

end module ModuleopenMPRefactor








