module mp_ml
  use inference_engine_m, only : inference_engine_t, tensor_t, tensor_map_t
  use sourcery_m, only : string_t, file_t
  use module_mp_simple, only : mp_simple_sediment
  implicit none

  private 
  public :: ml_mp_init, ml_mp

  type(inference_engine_t) network

contains

  subroutine ml_mp_init(network_file)
    type(string_t), intent(in) :: network_file
    network = inference_engine_t(file_t(network_file))
  end subroutine

  pure subroutine ml_mp( &
    press, th, temp, qv, qc, qr, qs, qi, dt_in, rho, rain, snow, dz, &
    its,ite, jts,jte, kts, kte, & ! tile dims
    ims,ime, jms,jme, kms, kme  & ! memory dims
  )
    integer, intent(in):: &
      ims,ime, jms,jme, kms,kme, &
      its,ite, jts,jte, kts,kte
    real, dimension(ims:ime, kms:kme, jms:jme), intent(inout):: qv, qr, qc, qi, th, qs, temp, press, rho, dz
    real, dimension(ims:ime, jms:jme), intent(inout):: rain, snow
    real, intent(in):: dt_in
    integer i, j, k, n
    type(tensor_t) outputs(its:ite, kts:kte, jts:jte)
    type(tensor_t) inputs( its:ite, kts:kte, jts:jte)

    do concurrent(i=its:ite, k=kts:kte, j=jts:jte)
      inputs(i,k,j) = tensor_t( &
        [press(i,k,j), th(i,k,j), temp(i,k,j), qv(i,k,j), qc(i,k,j), qr(i,k,j), qs(i,k,j)] &
      )
    end do

    inputs(its:ite, kts:kte, jts:jte) = network%map_to_input_range(inputs(its:ite, kts:kte, jts:jte))

    outputs(its:ite, kts:kte, jts:jte) = network%infer(inputs(its:ite, kts:kte, jts:jte))

    outputs(its:ite, kts:kte, jts:jte) = network%map_from_output_range(outputs(its:ite, kts:kte, jts:jte))

    do concurrent(i = its:ite, k = kts:kte, j = jts:jte)
      associate(output_tensor => outputs(i,k,j)%values())
        th(i,k,j)    = max(0.,    th(i,k,j) + output_tensor(1)*dt_in)
        qv(i,k,j)    = max(0.,    qv(i,k,j) + output_tensor(2)*dt_in)
        qr(i,k,j)    = max(0.,    qr(i,k,j) + output_tensor(3)*dt_in)
        qc(i,k,j)    = max(0.,    qc(i,k,j) + output_tensor(4)*dt_in)
        qs(i,k,j)    = max(0.,    qs(i,k,j) + output_tensor(5)*dt_in)
      end associate
    end do

    do concurrent(i = its:ite, j = jts:jte)
      call mp_simple_sediment( &
        press(i,:,j), temp(i,:,j), rho(i,:,j), qv(i,:,j), qc(i,:,j), qr(i,:,j), qs(i,:,j), rain(i,j), snow(i,j), &
        dt_in, dz(i,:,j), kms, kme, kts, kte, sediment_flag = .true. &
      )
    end do
  end subroutine

end module mp_ml
