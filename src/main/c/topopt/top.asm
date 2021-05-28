//apply density filter (gradient) only works on the very adjacent values
//gets contribution 1.5 from itself
//gets contribution 0.5 from all elems with neighbouring faces
//gets contribution 0.08 from all ements with neighbouring edges
//gets contribution 0 from all elems with neighbouring corners
{
    const FW_CENTRAL 1.5
    const FW_FACE 0.5
    const FW_EDGE 0.08

    //Load scalar values for later computation
    pstart single
    estart
    add.is s7, s0, FW_CENTRAL //filterweight for myself
    add.is s8, s0, FW_FACE //filterweight for face neighbours
    add.is s9, s0, FW_EDGE //filterweight for edge neighbours
    eend
    pend

    //perform computation
    pstart nelem (ijk)
    ld.fcn x1, X  //x1 = face neighbour values
    ld.edn1 x2, X //x2 = edge neighbour value
    ld.edn2 x3, X //x3 = edge neighbour values
    ld.sel x4, X	//x1 = central element value
    estart
        mul.ix x1, x1, FW_FACE //multiply face-neighbours with filterweights
        mul.ix x2, x2, FW_EDGE //multiply edge-neighbours with filterweights
        mul.ix x3, x3, FW_EDGE //multiply more edge-neighbours with filterweights
        mul.ix x4, x4, FW_CENTRAL //multiply central element with filterweight
        mac.ix s2, x1, 1 //sum all face neighbours
        mac.ix s3, x2, 1 //sum all edge neighbours
        mac.ix s4, x3, 1 //sum more edge neighbours
        mac.ix s5, x4, 1 //Sum central cell
        add.ss s3, s2, s3 //sum face and some edge neighbours
        add.ss s3, s3, s4 //add remaining edge neighbours
        add.ss s3, s3, s5 //add central element, s3=out[e1]
        uns.ss s4, x1 //s4 = unityScale[x1]
        div.ss s3, s3, s4 // out[e1] /= unityScale
        mul.sx x1, s0, x1 //Zero out x1
        add.sx x1, x3, x1 //Copy s-value into x1 for storage
    eend
    st.sel x1, XPHYS
    pend
}

//Apply density filter gradient
{
    const FW_CENTRAL = 1.5
    const FW_FACE = 0.5
    const FW_EDGE = 0.08

    //Load scalar values for later computation
    pstart single
    estart
    add.is s1, s0, 1 //constant 1
    add.is s7, s0, FW_CENTRAL //filterweight for myself
    add.is s8, s0, FW_FACE //filterweight for face neighbours
    add.is s9, s0, FW_EDGE //filterweight for edge neighbours
    eend
    pend

    //perform computation 1
    pstart nelem (ijk)
    ld.sel s1, X //s1 = v[e1]
    estart
    uns.ss s4, x1 //s4 = unityScale
    div.ss s1, s1, s4 //tmp[e1] = v[e1] / unityScale
    eend
    st.sel s1, TMP

    //perform computation 2
    pstart nelem (ijk)
    ld.fcn x1, TMP
    ld.edn x2, TMP
    ld.sel s1, TMP
    estart
    mul.ix x1, x1, FW_FACE //multiply face-neighbours with filterweights
    mul.ix x2, x2, FW_EDGE //multiply edge-neighbours with filterweights
    mul.is s1, s1, FW_CENTRAL //multiply central with filterweight
    mac.sx s2, x3, s1 //sum all face neighbours
    mac.sx s3, x4, s1 //sum all edge neighbours
    add.ss s3, s2, s3 //sum all neighbours
    add.ss s3, s3, s1 //add central element, s3=v[e1]
    eend
    st.sel s3, V
}

//applystateoperator
//elements: X
//dofs: U, output: Q
{
    pstart single
    estart
    //something to set s1 = emin = 1e-6
    add.is s1, s0, 1e-6
    add.is s2, s0, 1
    eend
    pend

    pstart nelem
    ld.dof v1, U
    ld.dof v2, Q
    ld.ele x1, X
    estart
    mul.xx x2, x1, x1 //x2 = pow(x,2)
    mul.xx x2, x2, x1 //x2 = pow(x,3)
    sub.ss s2, s2, s1 //s2 = e0-emin
    mul.sx x2, s2, x2 //x2 = pow(x,3)*(e0-emin)
    add.sx x2, s1, x2 //x2 = emin + pow(x,3)*(e0-emin)
    mul.xv v1, x2, v1 //v1 = u_local*elementScale
    mac.kv v1, v1 //v1 = ke*(u_local*elementscale)
    add.vv v2, v1, v2 //edof[i] += out_local[i]
    eend
    st.dof v2, Q
    pend
}

//generate matrix diagonal
{
    pstart single
    ld.vec v1, KEDIAG
    estart
    //something to set s1 = emin = 1e-6
    add.is s1, s0, 1e-6 //emin
    add.is s2, s0, 1 //e0
    eend
    pend

    pstart nelem
    ld.dof v2, INVD
    ld.ele x1, X
    estart
    mul.xx x2, x1, x1 //x2 = pow(x,2)
    mul.xx x2, x2, x1 //x2 = pow(x,3)
    sub.ss s2, s2, s1 //s2 = e0-emin
    mul.sx x2, s2, x2 //x2 = pow(x,3)*(e0-emin)
    add.sx x2, s1, x2 //x2 = emin + pow(x,3)*(e0-emin)
    mul.xv v1, x2, v1 //v1 = elementscala * ke[ii][ii]
    add.vv v2, v1, v2 //diag[edof[ii]] += eS * ke[ii][ii]
    eend
    st.dof v2, INVD
}
//Get compliance and sensitivity
//Inputs: X (elems), U (dof)
//Outputs: C (scalar), total change in design variables)
//         dc/dx (elems) dc/dx_e for all elements in the grid
{
    const PENAL 3
    const E0 1
    const EMIN 1e-6

    pstart nelem (ijk)
    ld.dof v1, U
    ld.ele x1, X
    estart
    mac.kv v2, v1 //tmp = ke*ulocal
    mac.vv x2, v1, v2 //clocal += u_local*tmp, mac into x-registers
    mul.xx x1, x1, x1 //x1 = pow(x,2)
    add.is s1, s0, -PENAL //s1 = -3
    add.is s2, s0, E0 //s2 = e0
    add.is s3, s0, EMIN //s3 = emin
    sub.ss s2, s2, s3 //s2 = e0-emin
    mul.ss s1, s2, s1 //s1 = -penal*(e0-emin)
    mul.sx x1, s1, x1 //x1 = -penal*(e0-emin)*pow(x,2)
    mul.xx x1, x2, x1 //x1 = clocal * (-penal*(e0-emin)*pow(x,2))
    eend
    st.elem x1, DCDX
}

//Precondition damped jacobi
//Inputs: U (vec), B (vec)
{
    const OMEGA 0.6
    const NSWP 2
    pstart single
    estart
    add.ss s1, s0, s0 //s1=0
    add.ss s3, s0, NSWP
    add.is s2, s0, OMEGA
    eend
    pend

    L1: applyStateOperator(gc, x, u, tmp)

    pstart ndof
    ld.vec v1, INVD
    ld.vec v2, B
    ld.vec v3, TMP
    ld.vec v4, U
    estart
    sub.vv v2, v2, v3 //v2 = b-tmp
    mul.vv v1, v1, v2 //v1 = invd*(b-tmp)
    mul.sv v1, s2, v1 //v1 = omega*(invd*(b-tmp))
    add.vv v4, v1, v4 //u += omega*(invd*(b-tmp))
    eend
    st.vec v4, U
    pend

    pstart single
    estart
    add.is s1, s0, 1
    eend
    pend

    bne s0, s3, L1 //Repeat for 2 iters
}

//Solve stage CG
{
    //Reset a lot of vectors
    pstart ndof
    estart
    mul.sv v0, s0, v0 //v0 = 0
    eend
    st.vec v0, R
    st.vec v0, Z
    st.vec v0, P
    st.vec v0, Q
    st.vec v0, INVD
    st.vec v0, TMP
    pend

    //Scalar registers in use
    //s1: bnorm
    //s2: omega = 0.6
    //s3: maxiter = 10000
    //s4: rho
    //s5: rhoold
    //s6: dpr
    //s7: alpha
    //s8: iter
    //s9: beta

    //Setup scalars
    const OMEGA 0.6
    const MAXITER 10000
    pstart single
    start
    add.ss s1, s0, s0 //bnorm=0
    add.is s2, s0, 0.6 //omega=0.6
    add.is s3, s0, 10000 //maxiter = 10000
    add.ss s4, s0, s0 //rho=0
    add.ss s5, s0, so //rhoold=0
    add.ss s6, s0, s0 //dpr=0
    add.ss s7, s0, s0 //alpha=0
    add.ss s8, s0, s0 //iter
    add.ss s9, s0, s0 //beta
    add.is 10, s0, 1e-5 //tolerance
    eend
    pend

    //Setup residual vector
    applyStateOperator(gc, x, u, r)
    pstart ndof
    ld.vec v0, B
    ld.vec v1, R
    estart
    sub.vv v0, v0, v1 //v0 = b-r
    eend
    st.vec v0, R
    pend

    //Setup inverse diagonal of system matrix
    pstart single
    estart
    add.is s10, s0, 1
    eend
    pend
    pstart nedof
    ld.vec v0, INVD
    estart
    div.sv v0, s10, v0 //v0 = 1/v0
    eend
    st.vec v0, INVD
    pend

    //Calculate bnorm
    pstart ndof
    ld.vec v0, B
    estart
    mac.vv s1, v0, v0
    eend
    pend
    pstart single
    estart
    sqrt s1, s1 //pseudo-instruction
    eend
    pend

    //Conjugate gradient main loop
    CGLOOP:
    pstart ndof //zero out z-vector
    estart
    mul.sv v0, s0, v0
    eend
    st.vec v0, Z

    preconditionedDampedJacobi(gc, x, nswp, omega, invd, z, r, tmp)

    pstart ndof //inner product of r and z
    ld.vec v0, R
    ld.vec v1, Z
    estart
    mac.vv s4, v0, v1
    eend
    pend


    beq s8, s0, RHOITER0
    pstart single //beta = rho/rhoold
    estart
    div.ss s9, s4, s5
    eend
    pend
    pstart ndof //p[i] = beta * p[i] + z[i]
    ld.vec v0, P
    ld.vec v1, Z
    estart
    mul.sv v0, s9, v0 //v0 = beta*p
    add.vv v0, v0, v1 //v0 = beta*p+z
    eend
    st.vec v0, P
    pend
    beq s0, s0, AFTERRHO

    RHOITER0
    pstart ndof //p[i] = z[i]
    ld.vec v0, Z
    estart
    eend
    st.vec v0, P
    pend

    AFTERRHO:
    applyStateOperator(gc, x, p, q)

    //Inner product of p and q
    pstart ndof
    ld.vec v0, P
    ld.vec v1, Q
    estart
    mac.vv s6, v0, v1 //dpr = innerProduct(p,q)
    eend
    pend

    pstart single //update alpha and rhoold
    estart
    div.ss s7, s4, s6 //alpha = rho/dpr
    add.ss s5, s0, s4 //rhoold = rho
    eend
    pend

    pstart ndof //update u and r vectors
    ld.vec v0, P
    ld.vec v1, Q
    ld.vec v2, U
    ld.vec v3, R
    estart
    mul.sv v0, s7, v0 //v0 = alpha*p
    mul.sv v1, s7, v1 //v1 = alpha*q
    add.vv v2, v2, v0 //v2 = u += alpha*p
    sub.vv v3, v3, v1 //v3 = r -= alpha*q
    eend
    st.vec v2, U
    st.vec v3, R
    pend

    pstart ndof //calculate relres
    ld.vec v0, R
    estart
    mac.vv s11, v0, v0
    end
    pend

    pstart single
    estart
    div.ss s11, s11, s1 //s11 = relres = norm(r) / bnorm
    add.is s8, s8, 1 //iter += 1
    eend
    pend

    beq s8, s3, ENDCG //end of for loop
    bge s11, s10, CGLOOP //wanted tolerance achived

    ENDCG:
}

//Top3dcg
{
    //reset F, U vectors
    pstart ndof
    estart
    mul.sv v0, s0, v0
    eend
    st.vec v0, F
    st.vec v0, U

    //populate design space
    pstart nelem (linear)
    estart
    add.iv v0, VOLFRAC
    add.iv v1, 1
    eend
    st.vec v0, X
    st.vec v0, XPHYS
    st.vec v1, DV

    applyDensityFilterGradient(dv)

    pstart single
    estart
    add.ss s1, s0, s0 //loop
    add.is s1, s0, 1 //change

    TOP3DCG:
    pstart single
    start
    add.is s1, s1, 1 //loop++
    eend
    pend

    solveStageCG()
    getComplianceAndSensitivity()
    applyDensityFilterGradient()

    pstart single //load 1 for next operation
    estart
    add.is s2, 0, 1 //s2=1
    eend
    pend

    pstart nelem (linear) //calculate sum of xphys
    ld.vec v0, XPHYS
    estart
    mac.iv s4, v0, 1 //sum(xphys)
    eend
    pend

    pstart single //calculate g and vol
    estart
    add.ss s5, s0, s4 //s5 = g = xphys
    add.ss s4, s0, s4 //s4 = vol = xphys
    add.is s6, s0, NELEM //s6 = nelem
    div.ss s5, s5, s6 //s5 = g/nelem
    div.ss s4, s4, s6 //s4 = vol/nelem
    sub.is s5, s5, VOLFRAC // s5 = g/nelem - volfrac
    eend
    pend

    pstart single //Setup l1, l2 values
    estart
    add.ss s9, s0, 1e-6 //tolerance
    add.ss s10, s0, s0 //l1 = 0
    add.ss s11, s0, 50000 //l2 = 50k
    add.ss s12, s0, 0.2 //move = 0.2
    add.ss s15, s0, -0.2 //-move
    eend
    pend

    //Calculate loop comparison
    LAGRANGESTART:
    pstart single
    estart
    sub.ss s13, s11, s10 //s13 =l2-l1
    add.ss s14, s11, s10 //s4 = l2+l1
    div.ss s13, s13, s14 //s13 = (l2-l1)/(l2+l1)
    eend
    pend
    blt s13, s9, LAGRANGEEND

    //Calculate lagrange update
    pstart single
    estart
    mul.is s13, s14, 0.5 //s13 = lmid = 0.5*(l2+l1)
    add.ss s14, s0, s0 //gt = 0

    eend
    pend

    pstart nelem (linear)
    ld.vec v0, X
    ld.vec v1, dc
    ld.vec v2, dv
    //v3 = xnew
    estart
    div.vv v1, v1, v2 //v1 = dc/dv
    mul.sv v1, s13, v1 //v1 = dc/dv * lmid
    mul.iv v1, v1, -1 //v1 = -dc/dv * lmid
    sqrt v1, v1 //v1 = sqrt(-dc/dv*lmid)
    add.sv v3, s12, v0 //v3 = x + move
    min.vv v1, v1, v3 //v1 = min(v1,v3)
    min.iv v1, v1, 1 //v1 = min(v1,1)
    add.sv v3, v0, s15 //v3 = x - move
    max.vv v1, v1, v3 //v1 = max(v1,v3)
    max.iv v1, v1, 0 //v1 =  max(0, v1)
    sub.vv v3, v1, v0 //v3 = xnew-x
    mac.vv s8, v2, v3 //s8 = dotProduct(dv, (xnew-x))
    add.ss s14, s14, s8 //gt += above

    blt s14, s0, GTNEG
    pstart single
    estart
    add.ss s10, s0, s13 //l1 = lmid
    eend
    pend
    beq s0, s0, LAGRANGESTART
    GTNEG:
    pstart single
    estart
    add.ss s11, s0, 13 //l2 = lmid
    eend
    pend
    beq s0, s0, LAGRANGESTART

    LAGRANGEEND:
    pstart single
    estart
    add.ss s11, s0, s0 //s11 = change = 0
    mul.sv v2, s0, v2 //v2 = 0
    add.is s12, s0, 1 //s12=1
    add.is s13, s0, (0.02*ELEMS_PER_VSLOT)
    eend
    pend

    pstart nelem
    ld.vec v0, X
    ld.vec v1, XNEW
    estart
    sub.vv v0, v0, v1 //v0 = x-xnew
    abs.vv v0, v0 //v0 = abs(x-xnew)
    max.vv v2, v0, v2 //v2 = max(change, abs(x-xnew))
    eend
    pend

    //To see if all 'change' value were less than 0.02,
    //we take max of all 'change' values and 0.02
    //if sum of vector is equal to ELEMS_PER_VSLOT*0.02, all values were less than 0.02
    pstart single
    estart
    max.sv v2, v2, 0.02
    mac.sv s13, s12, v2 //sum all values in v2
    eend
    pend

    applyDensityFilter

    pstart single
    estart
    add.is s14, s0, 0.02
    add.is s15, s0, 100
    pend
    eend

    beq s13, s12, END //If equal, all changes were less than tolerance
    beq s1, s15, END //if equal, we've run the max number of iterations
    beq s0, s0, TOP3DCG //if neither branc was taken, go back
    END:
}





