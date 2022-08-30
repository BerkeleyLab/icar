module mp_ml
  use assert_m, only : assert
  !use nf, only : network
  implicit none

  private 
  public :: ml_init

  !type(network) :: qr_net

contains

  subroutine ml_init(file_name, inweights, aiweights, outweights)
    character(len=*), intent(in) :: file_name
    real, allocatable, intent(out) :: inweights(:,:)   ! input layer, e.g., 10 weights/node x 64 nodes
    real, allocatable, intent(out) :: aiweights(:,:,:) ! hidden layers, e.g., 64 nodes x 64 weights/node x 50 layers
    real, allocatable, intent(out) :: outweights(:,:)  ! output layer: 1 output x 64 weights (Qr = (kg H20(liq.))/(kg Air))

    integer num_inputs, num_nodes, num_layers, num_outputs
    integer i, j, file_unit, stat

    open(newunit=file_unit, file=file_name, form='formatted', status='old', iostat=stat, action='read')      
    call assert(stat==0,"stat==0 in open")

    read(file_unit,*,iostat=stat) num_inputs, num_nodes, num_layers, num_outputs
    call assert(stat==0,"stat==0 in read(...) num_inputs,...")

    allocate(inweights(num_inputs, num_nodes), aiweights(num_nodes, num_nodes, num_layers), outweights(num_outputs, num_nodes))

    do i = 1,size(inweights,1)
      read(file_unit, *, iostat=stat) inweights(i,:)
      call assert(stat==0,"stat==0 in read(...) inweights")
    end do

    do i = 1,size(aiweights,1)
      do j = 1,size(aiweights,3)
        read(file_unit, *, iostat=stat) aiweights(i,:,j)
        call assert(stat==0,"stat==0 in read(...) aiweights")
      end do
    end do

    do i = 1, size(outweights,2)
      read(file_unit, *, iostat=stat) outweights(:,i)
      call assert(stat==0,"stat==0 in read(...) outweights")
    end do

    close (file_unit)

  end subroutine

end module
