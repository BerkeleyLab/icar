submodule(grid_interface) grid_implementation
    use assertions_mod,       only : assert, assertions
    implicit none


contains

    module function get_dims(this) result(dims)
        class(grid_t) :: this
        integer :: dims(3)

        dims(1) = this%ime - this%ims + 1
        dims(2) = this%kme - this%kms + 1
        dims(3) = this%jme - this%jms + 1
    end function

    !> -------------------------------
    !! Decompose the domain into as even a set of tiles as possible in two dimensions
    !!
    !! Searches through possible numbers of x and y tiles that multiple evenly to
    !! give the total number of images requested.
    !!
    !! For each x/y split compute the number of grid cells in both dimensions in each tile
    !! return the split that provides the closest match between the number of x and y grid cells
    !!
    !! -------------------------------
    module subroutine domain_decomposition(this, nx, ny, nimages, ratio)
        class(grid_t),  intent(inout) :: this
        integer,        intent(in)    :: nx, ny, nimages
        real,           intent(in), optional :: ratio
        real    :: multiplier
        integer :: ysplit, xsplit, xs, ys, i
        real    :: best, current, x, y

        multiplier=1
        if (present(ratio)) multiplier = ratio

        xsplit = 1
        ysplit = nimages
        xs = xsplit
        ys = ysplit

        x = (nx/real(xsplit))
        y = (ny/real(ysplit))

        if (y > (multiplier*x)) then
            best = abs(1 - ( y / (multiplier*x) ))
        else
            best = abs(1 - ( (multiplier*x) / y ))
        endif

        do i=nimages,1,-1
            if (mod(nimages,i)==0) then
                ysplit = i
                xsplit = nimages / i

                x = (nx/float(xsplit))
                y = (ny/float(ysplit))

                if (y > (multiplier*x)) then
                    current = abs(1 - ( y / (multiplier*x) ))
                else
                    current = abs(1 - ( (multiplier*x) / y ))
                endif

                if (current < best) then
                    best = current
                    xs = xsplit
                    ys = ysplit
                endif
            endif
        enddo

        this%ximages = xs
        this%yimages = ys

        this%ximg = mod(this_image()-1,  this%ximages)+1
        this%yimg = floor(real(this_image()-1) / this%ximages)+1

        x = (nx/float(xs))
        y = (ny/float(ys))

        if (assertions) call assert((xs*ys) == nimages, "Number of tiles does not sum to number of images")
        if (this_image()==1) print*, "ximgs=",xs, "yimgs=",ys

    end subroutine domain_decomposition

    !> -------------------------------
    !! Compute the number of grid cells in the current image along a dimension
    !!
    !! This takes care of the fact that generally the number of images will not evenly divide the number of grid cells
    !! In this case the extra grid cells need to be evenly distributed among all images
    !!
    !! n_global should be the full domain size of the dimension
    !! me should be this images image number along this dimension
    !! nimg should be the number of images this dimension will be divided into
    !!
    !! -------------------------------
    function my_n(n_global, me, nimg) result(n_local)
       integer, intent(in) :: n_global, me, nimg
       integer :: n_local

       ! add 1 if this image is less than the remainder that need an extra grid cell
       n_local = n_global / nimg + merge(1,0,me <= mod(n_global,nimg)  )
    end function

    function my_start(n_global, me, nimg) result(memory_start)
        implicit none
        integer, intent(in) :: n_global, me, nimg
        integer :: memory_start
        integer :: base_n

        base_n = n_global / nimg

        memory_start = (me-1)*(base_n) + min(me-1,mod(n_global,nimg)) + 1

    end function my_start

    ! Generate the domain decomposition mapping and compute the indicies for local memory
    module subroutine get_grid_dimensions(this, nx, ny, nz, nx_extra, ny_extra)
      class(grid_t),   intent(inout) :: this
      integer,         intent(in)    :: nx, ny, nz
      integer,         intent(in), optional :: nx_extra, ny_extra

      integer :: nx_e, ny_e

      nx_e = 0
      ny_e = 0
      if (present(nx_extra)) nx_e = nx_extra ! used to add 1 to the u-field staggered grid
      if (present(ny_extra)) ny_e = ny_extra ! used to add 1 to the v-field staggered grid

      call this%domain_decomposition(nx, ny, num_images())

      this%ny_global  = ny                                            ! global model domain grid size
      this%nx_global  = nx                                            ! global model domain grid size
      this%nz         = nz                                            ! note nz is both global and local
      this%nx         = my_n(this%nx_global, this%ximg, this%ximages) ! local grid size
      this%ny         = my_n(this%ny_global, this%yimg, this%yimages) ! local grid size

      ! define the bounds needed for memory to store the data local to this image
      this%ims        = my_start(this%nx_global, this%ximg, this%ximages)
      this%ime        = this%ims + this%nx + nx_e - 1

      this%jms        = my_start(this%ny_global, this%yimg, this%yimages)
      this%jme        = this%jms + this%ny + ny_e - 1

      this%kms        = 1
      this%kme        = this%nz

      ! Now define the tile of data to process in physics routines
      this%its = this%ims + 1
      this%jts = this%jms + 1
      this%kts = this%kms
      this%ite = this%ime
      this%jte = this%jme - 1
      this%kte = this%kme - 1

      ! The entire model domain begins at 1 and ends at nx,y,z
      this%ids = 1
      this%jds = 1
      this%kds = 1
      this%ide = this%nx_global
      this%jde = this%ny_global
      this%kde = this%nz

      ! define the halo needed to manage communications between images
      this%ns_halo_nx = this%nx_global / this%ximages + 1 + nx_e  ! number of grid cells in x in the ns halo
      this%ew_halo_ny = this%ny_global / this%yimages + 1 + ny_e  ! number of grid cells in y in the ew halo
      this%halo_nz    = this%nz

  end subroutine


end submodule
