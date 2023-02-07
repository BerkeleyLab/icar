module mp_ml
  use inference_engine_m, only : inference_engine_t
  use string_m, only : string_t
  use sigmoid_m, only : sigmoid_t
  use file_m, only : file_t
  implicit none

  private 
  public :: ml_init, ml_mp

  type(inference_engine_t), allocatable :: networks(:)

contains

  subroutine ml_mp_init(network_files)
    type(string_t), intent(in) :: network_files(:)

    networks = inference_engine_t(file_t(network_files), sigmoid_t())
  end subroutine

  pure function ml_mp( &
    qv, qr, qc, ni, th, nr, qs, qg, temp, press, dt_in, &
    its,ite, jts,jte, kts,kte & ! tile dims
    ims,ime, jms,jme, kms,kme & ! tile dims
  ) result(outputs)              
    integer, intent(in):: ims,ime, jms,jme, kms,kme, its, ite, jts, jte, kts, kte
    real, dimension(ims:ime, kms:kme, jms:jme), intent(inout):: qv, qr, qc, ni, th, nr, qs, qg, temp, press
    real, dimension(ims:ime, kms:kme, jms:jme), intent(in):: p
    real, intent(in):: dt_in

    real outputs(its:ite, jts:jte, size(networks))

    do concurrent(i = its:ite, j = jts:jte, k=1:size(networks))
      outputs(i,k,j) = networks(k)%infer( &
        inputs = inputs_t( &
         qv(i,:,j), qr(i,:,j), qc(i,:,j), ni(i,:,j), th(i,:,j), nr(i,:,j), qs(i,:,j), qg(i,:,j), temp(i,:,j), press(i,:,j), dt_in &
      ) )
    end do
  end function

end module mp_ml
