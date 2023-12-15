! Advent of Code 2023, Day 10 - Pipe Maze.
! Fortran.

! Store type info.
module PipeModule
    implicit none

    ! Define a custom type for Pipe segments.
    integer, parameter :: maxRows = 140
    integer, parameter :: maxCols = 140

    type :: Pipe
        character(len=1) :: segment
        integer :: distance        
    end type Pipe

end module PipeModule

! Main program
program PipeMaze
    use PipeModule
    implicit none

    character(len=50) :: inputFile = 'Day10Input.txt'
    type(Pipe), dimension(maxRows,maxCols) :: pipeGrid

    call ParseData(inputFile, pipeGrid)
    call PrintGrid(pipeGrid)

end program pipeMaze

! Parse input data into 2D array of Pipe segments.
subroutine ParseData(inputFile, pipeGrid)
    use PipeModule
    implicit none

    character(len=*), intent(in) :: inputFile
    type(Pipe), dimension(maxRows,maxCols), intent(out) :: pipeGrid
    character(len=140) :: line 
    integer :: r, c, ioStatus

    open (1, file = inputFile, status = 'old')

    r = 0
    do
        read(1,*, iostat=ioStatus) line
        if (ioStatus == -1) exit
        r = r + 1
        do c = 1, maxCols            
            pipeGrid(r,c)%segment = line(c:(c+1)) 
            pipeGrid(r,c)%distance = 0
        end do
    end do

end subroutine ParseData

! Print the pipe grid contents.
subroutine PrintGrid(pipeGrid)
    use PipeModule
    implicit none

    integer :: r, c
    type(Pipe), dimension(maxRows,maxCols), intent(in) :: pipeGrid

    do r = 1, maxRows
        do c = 1, maxCols
            write(*, '(A)', advance='no') pipeGrid(r,c)%segment
        end do
        print *,""
    end do

end subroutine PrintGrid