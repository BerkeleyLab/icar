module mp_ml
  use inference_engine_m, only : inference_engine_t, outputs_t, inputs_t
  use string_m, only : string_t
  use sigmoid_m, only : sigmoid_t
  use file_m, only : file_t
  implicit none

  private 
  public :: ml_mp_init, ml_mp

  type(inference_engine_t), allocatable :: networks(:)

contains

  subroutine ml_mp_init(network_files)
    type(string_t), intent(in) :: network_files(:)

    networks = inference_engine_t(file_t(network_files), sigmoid_t())
  end subroutine

  pure subroutine ml_mp( &
    qv, qr, qc, ni, th, nr, qs, qg, temp, press, dt_in, &
    its,ite, jts,jte, kts,kte, & ! tile dims
    ims,ime, jms,jme, kms,kme  & ! memory dims
  )
    integer, intent(in):: &
      ims,ime, jms,jme, kms,kme, &
      its,ite, jts,jte, kts,kte
    real, dimension(ims:ime, kms:kme, jms:jme), intent(inout):: qv, qr, qc, ni, th, nr, qs, qg, temp, press
    real, intent(in):: dt_in
    integer i, j, k, n
    integer, parameter :: num_networks = 8
    type(outputs_t) outputs(its:ite, jts:jte, num_networks)

    do concurrent(i = its:ite, j = jts:jte, n=1:num_networks)
      outputs(i,j,n) = networks(n)%infer( &
        inputs = inputs_t( &
         qv(i,:,j), qr(i,:,j), qc(i,:,j), ni(i,:,j), th(i,:,j), nr(i,:,j), qs(i,:,j), qg(i,:,j), temp(i,:,j), press(i,:,j) &
      ) )
    end do
    do concurrent(i = its:ite, j = jts:jte)
      qv(i,:,j) = qv(i,:,j) + outputs(i,j,1)%outputs_(:)*dt_in
      qr(i,:,j) = qr(i,:,j) + outputs(i,j,2)%outputs_(:)*dt_in
      qc(i,:,j) = qc(i,:,j) + outputs(i,j,3)%outputs_(:)*dt_in
      ni(i,:,j) = ni(i,:,j) + outputs(i,j,4)%outputs_(:)*dt_in
      th(i,:,j) = th(i,:,j) + outputs(i,j,5)%outputs_(:)*dt_in
      nr(i,:,j) = nr(i,:,j) + outputs(i,j,6)%outputs_(:)*dt_in
      qs(i,:,j) = qs(i,:,j) + outputs(i,j,7)%outputs_(:)*dt_in
      qg(i,:,j) = qg(i,:,j) + outputs(i,j,8)%outputs_(:)*dt_in
    end do
  end subroutine

end module mp_ml
