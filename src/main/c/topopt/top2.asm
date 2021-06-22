//This file contains much of the same content as top.asm, but contains the instructions in the correct order for execution

//This is where all the setup in top3dcg is performed
//populate design space
//Volfrac = 0.2
pstart nelemvec
estart
mul.sv v0, s0, v0 //clear v0
mul.sv v1, s0, v1 //Clear v1
add.iv v0, v0, 0.2 //v0 = 0.2
add.iv v1, v1, 1 //v1 = 1
eend
st.vec v0, X
st.vec v0, XPHYS
st.vec v1, DV
pend

//Apply density filter gradient on DV
//{
    //For each element, calculate unityscale and scale the central element by this value. Store that value in tmp
    //Unityscale is obtained by getting all neighbours, dividing by themselves to get 1, and then multiplying
    //neighbours with their known, constant filterweight. Summing all filterweights gives unityscale
    pstart single //Initial setup
    estart
    mul.sx x0, s0, x0 //clear x0
    add.ix x0, x0, 1 //Set x0=1
    eend
    pend

    pstart nelemstep //Step through all elements
    ld.sel x1, DV //x0 = v[e1]
    ld.fcn x2, DV //x1 = fcn[e1]
    ld.edn1 x3, DV //x2 = edn1[e1]
    ld.edn2 x4, DV //x3 = edn2[e1]
    estart
    div.xx x2, x2, x2 //x2 = x2/x2 (all non-zero elements become 1)
    div.xx x3, x3, x3 //x3 = x3/x3
    div.xx x4, x4, x4 //x4 = ones
    mul.ix x2, x2, 0.5 //face neighbour weight = 0.5
    mul.ix x3, x3, 0.08578 //Edge neighbour weight = 0.08578
    mul.ix x4, x4, 0.08578 //edge neighbour weight = 0.08578
    red.xx s1, x0, x2 //Sum all face neighbour weightings
    red.xx s2, x0, x3 //Sum some edge neigbour weights
    add.ss s1, s1, s2 //Add partial sum
    red.xx s2, x0, x4 //Sum remaining edge neighbour weights
    add.ss s1, s1, s2 //Add remaining edge neighbours
    add.is s1, s1, 1.5 //Add central element weighting. s1 = unityscale
    div.sx x1, s1, x1 //x1 = v[e1]/unityscale
    eend
    st.sel x1, TMP
    pend

    //For each element in DV, clear its value. Then loop through neighbourhood, setting new element
    //value to be a scaled version of all input values in tmp
    pstart nelemstep //Through all elements
    ld.sel x1, TMP
    ld.fcn x2, TMP
    ld.edn1 x3, TMP
    ld.edn2 x4, TMP
    estart //We need both the original values AND count of values
    mul.ix x2, x2, 0.5 //scale face neighbours
    mul.ix x3, x3, 0.08578 //Scale edge neighbours
    mul.ix x4, x4, 0.08578 //Scale edge neighbours
    mul.ix x1, x1, 1.5 //Scale central element
    red.xx s1, x1, x0 //Sum values in x1
    red.xx s2, x2, x0 //Sum values in x2
    add.ss s1, s1, s2
    red.xx s2, x3, x0 //Sum values in x3
    add.ss s1, s1, s2
    red.xx s2, x4, x0 //Sum values in x4
    add.ss s1, s1, s2 //s1 = v[e1] += filterWeight*tmp[e2] for all neighbours
    div.xx x1, x1, x1 //Reset value in x1 to a single value
    mul.sx x1, s1, x1 //Set first value in x1 to new element value
    eend
    st.sel x1, DV
    pend
//}

//Setup loop and change variables
pstart single
estart
add.ss s15, s0, s0 //s0=loop=0
add.is s14, s0, 1 //s1=change=1
eend
pend

//Main top3dcg loop starts here
TOP3DCG:
pstart single
estart
add.is s15, s0, 1 //loop++
eend
pend

//Solve state conjugate gradient
// Works on xphys, F and U
//Main function performing the majority of computations.
{


    //Reset a lot of vectors
    pstart single
    estart
    mul.sv v0, s0, v0
    eend
    pend

    pstart ndof
    estart
    eend
    st.vec v0, R
    st.vec v0, Z
    st.vec v0, P
    st.vec v0, Q
    st.vec v0, INVD
    st.vec v0, TMP
    pend

    //Apply state operator with xphys as densities, U as input values and R as output
    //Setup constant values
    pstart single
    estart
    add.is s1, s0, 0.0078125
    mul.ss s1, s1, s1 // s2 =6.1e-5
    add.is s2, s0, 0.015625
    mul.ss s2, s1, s2 //s2 =9.5e-7 ~= 1e-6 = emin
    add.is s1, s0, 1 //s1=1=E0
    sub.ss s1, s1, s2 // s1 = e0-emin
    mul.sv v0, s0, v0 //Clear v0
    eend
    pend

    //Clear output vector
    pstart ndof
    estart
    eend
    st.vec v0, R
    pend

    pstart nelemdof //Loop over elements and perform applystateoperator
    ld.dof v0, U
    ld.dof v1, R
    ld.elem x0, XPHYS
    estart
    mul.xx x1, x0, x0 //x0 = pow(x,2)
    mul.xx x1, x1, x0 //x1 = pow(x,3)
    mul.sx x1, s1, x1 //x1 = pow(x,3)*(e0-emin)
    add.sx x1, s2, x1 //x1 = emin+pow(x,3)*(e0-emin)
    mul.xv v2, x1, v0 //v2 = u_local*elementScale
    mac.kv v2, v2 //ke*(u_local*elementScale)
    add.vv v2, v2, v1 //out[edof[i]] += out_local[i]
    eend
    st.dof v2, R
    st.fdof v0, R //Storing fixed DOFs with input values from U
    pend
    //End of apply state operator

    //Calculate value of r[i]=b[i]-r[i], b=F
    pstart ndof
    ld.vec v0, R
    ld.vec v1, F
    estart
    sub.vv v0, v1, v0 //r[i] = b[i]-r[i]
    eend
    st.vec v0, R
    pend

    //Generate matrix diagonal
    //Similar to apply state operator

    //Setup constant values
    pstart single
    estart
    add.is s1, s0, 0.0078125
    mul.ss s1, s1, s1 // s2 =6.1e-5
    add.is s2, s0, 0.015625
    mul.ss s2, s1, s2 //s2 =9.5e-7 ~= 1e-6 = emin
    add.is s1, s0, 1 //s1=1=E0
    sub.ss s1, s1, s2 // s1 = e0-emin
    mul.sv v0, s0, v0 //Clear v0
    add.iv v1, v0, 1 //v1 = 1 = fixed dofs for diag
    add.iv v2, v0, 0.235043 //ke diagonal value
    eend
    pend

    pstart ndof
    estart
    eend
    st.vec v0, INVD
    pend

    pstart nelemdof //Main loop, generate KE diag
    ld.dof v0, INVD
    ld.elem x0, XPHYS
    estart
    mul.xx x1, x0, x0 //x1 = pow(x,2)
    mul.xx x1, x1, x0 //x1 = pow(x,3)
    mul.sx x1, s1, x1 //x1 = powe(x,3)*(e0-emin)
    add.sx x1, s2, x1 //x1 = emin + pow(x,3)*(e0-emin) = elementScale
    mul.xv v2, x1, v0 //v2 = elementscale * ke[ii][ii]
    add.vv v0, v0, v2 //diag[edof[ii]] += elementScale*ke[ii][ii]
    eend
    st.dof v0, INVD //Store dofs
    st.fdof v1, INVD //Store fixed dofs = 1
    pend

    //Invert values
    pstart ndof
    ld.vec v0, INVD
    estart
    div.iv v0, v0, 1 //v0 = 1/invD[i]
    eend
    st.vec v0, INVD
    pend

    //Setup scalars for cg loop
    pstart single
    estart
    //s13 = bnorm, setting below
    add.ss s12, s0, s0 //rho
    add.ss s11, s0, s0 //rhoold
    add.ss s10, s0, s0 //dpr
    add.ss s9, s0, s0 //alpha
    add.ss s8, s0, s0 //cgiter
    add.is s7, s0, 5 //s7 = 5
    mul.is s7, s7, 5 //s7 = 25
    mul.is s7, s7, 5 //s7 = 125
    mul.is s7, s7, 5 //s7 = 625
    mul.is s7, s7, 4 //s7 = 2500
    mul.is s7, s7, 4 //s7 = 10000 = maxiter
    //s6 = tolerance, setting below
    eend
    pend


    { //Calculate bnorm = norm of F vector
        pstart ndof //Calculate sum(F[i]*F[i])
        ld.vec v0, F
        estart
        mac.iv s1, v0, 1
        eend
        pend

        //Calculate square root of sum to get norm
        pstart single
        estart
        //s1 holds S value
        mul.is s2, s1, 0.5 //s2 = xn = S/2
        add.is s3, s0, 0.5 //s3 = constant 1/2
        //Start looping
        div.ss s4, s1, s2 //s4 = S/xn
        add.ss s4, s4, s2 //s4 = xn + S/xn
        mul.ss s2, s3, s4 //s2 = xnew = 1/2*(xn+S/xn) //First loop
        div.ss s4, s1, s2 //s4 = S/xn
        add.ss s4, s4, s2 //s4 = xn + S/xn
        mul.ss s2, s3, s4 //s2 = xnew = 1/2*(xn+S/xn) //Second loop
        div.ss s4, s1, s2 //s4 = S/xn
        add.ss s4, s4, s2 //s4 = xn + S/xn
        mul.ss s2, s3, s4 //s2 = xnew = 1/2*(xn+S/xn) //Third iteration
        div.ss s4, s1, s2 //s4 = S/xn
        add.ss s4, s4, s2 //s4 = xn + S/xn
        mul.ss s2, s3, s4 //s2 = xnew = 1/2*(xn+S/xn) //Fourth iteration
        div.ss s4, s1, s2 //s4 = S/xn
        add.ss s4, s4, s2 //s4 = xn + S/xn
        mul.ss s13, s3, s4 //s13 = sqrt(S) = bnorm  //Fifth iteration
        //s2 = sqrt(x1)
        eend
        pend
    }

    { //Set tolerance value
        pstart single
        estart
        add.is s6, s0, 0.0078125
        mul.ss s6, s6, s6 //s6 = 6.1e-5
        mul.is s6, s6, 0.1640625 //s6 ~= 1e-5 = tolerance
        eend
        pend
    }

    CGMAINLOOP:
    pstart single //Reset z vector
    estart
    mul.sv v0, s0, v0 //Reset v0
    eend
    pend

    //Reset z vector
    pstart ndof
    estart
    eend
    st.vec v0, Z
    pend

    //Precondition damped jacobi
    //nswp = 2. Just going to copy/paste the instructions twice
    {
        //First iteration
        //Applystateoperator with xphys, U as input and TMP as output

        //Setup constant values
        pstart single
        estart
        add.is s1, s0, 0.0078125
        mul.ss s1, s1, s1 // s2 =6.1e-5
        add.is s2, s0, 0.015625
        mul.ss s2, s1, s2 //s2 =9.5e-7 ~= 1e-6 = emin
        add.is s1, s0, 1 //s1=1=E0
        sub.ss s1, s1, s2 // s1 = e0-emin
        mul.sv v0, s0, v0 //Clear v0
        eend
        pend

        //Clear output vector
        pstart ndof
        estart
        eend
        st.vec v0, TMP
        pend

        pstart nelemdof //Loop over elements and perform applystateoperator
        ld.dof v0, U
        ld.dof v1, TMP
        ld.elem x0, XPHYS
        estart
        mul.xx x1, x0, x0 //x0 = pow(x,2)
        mul.xx x1, x1, x0 //x1 = pow(x,3)
        mul.sx x1, s1, x1 //x1 = pow(x,3)*(e0-emin)
        add.sx x1, s2, x1 //x1 = emin+pow(x,3)*(e0-emin)
        mul.xv v2, x1, v0 //v2 = u_local*elementScale
        mac.kv v2, v2 //ke*(u_local*elementScale)
        add.vv v2, v2, v1 //out[edof[i]] += out_local[i]
        eend
        st.dof v2, TMP
        st.fdof v0, TMP //Storing fixed DOFs with input values from U
        pend
        //End of apply state operator

        //Update values of u
        pstart ndof
        ld.vec v0, F
        ld.vec v1, TMP
        ld.vec v2, INVD
        ld.vec v3, U
        estart
        sub.vv v0, v0, v1 //v0 = F[i]- tmp[i]
        mul.vv v0, v0, v2 //v0 = invD[i]*(F[i]-tmp[i])
        mul.iv v0, v0, 0.6 //v0 = omega*invD[i]*(F[i]-tmp[i])
        add.vv v0, v0, v3 //v0 = u[i] += omega*invD[i]*(F[i]-tmp[i])
        eend
        st.vec v0, U
        pend

        //Second iteration
        //Applystateoperator with xphys, U as input and TMP as output

        //Setup constant values
        pstart single
        estart
        add.is s1, s0, 0.0078125
        mul.ss s1, s1, s1 // s2 =6.1e-5
        add.is s2, s0, 0.015625
        mul.ss s2, s1, s2 //s2 =9.5e-7 ~= 1e-6 = emin
        add.is s1, s0, 1 //s1=1=E0
        sub.ss s1, s1, s2 // s1 = e0-emin
        mul.sv v0, s0, v0 //Clear v0
        eend
        pend

        //Clear output vector
        pstart ndof
        estart
        eend
        st.vec v0, TMP
        pend

        pstart nelemdof //Loop over elements and perform applystateoperator
        ld.dof v0, U
        ld.dof v1, TMP
        ld.elem x0, XPHYS
        estart
        mul.xx x1, x0, x0 //x0 = pow(x,2)
        mul.xx x1, x1, x0 //x1 = pow(x,3)
        mul.sx x1, s1, x1 //x1 = pow(x,3)*(e0-emin)
        add.sx x1, s2, x1 //x1 = emin+pow(x,3)*(e0-emin)
        mul.xv v2, x1, v0 //v2 = u_local*elementScale
        mac.kv v2, v2 //ke*(u_local*elementScale)
        add.vv v2, v2, v1 //out[edof[i]] += out_local[i]
        eend
        st.dof v2, TMP
        st.fdof v0, TMP //Storing fixed DOFs with input values from U
        pend
        // End of apply state operator

        //Update values of u
        pstart ndof
        ld.vec v0, F
        ld.vec v1, TMP
        ld.vec v2, INVD
        ld.vec v3, U
        estart
        sub.vv v0, v0, v1 //v0 = F[i]- tmp[i]
        mul.vv v0, v0, v2 //v0 = invD[i]*(F[i]-tmp[i])
        mul.iv v0, v0, 0.6 //v0 = omega*invD[i]*(F[i]-tmp[i])
        add.vv v0, v0, v3 //v0 = u[i] += omega*invD[i]*(F[i]-tmp[i])
        eend
        st.vec v0, U
        pend
    } //End of precondition damped jacobi

    //Inner product of r and z
    pstart ndof
    ld.vec v0, R
    ld.vec v1, Z
    estart
    mac.vv s12, v0, v1 //s12 = rho = dpr(r,z)
    eend
    pend

    bne s8, s0 CG_OTHER_ITERATIONS //jump if iter != 0
    //Set p[i] = z[i]
    pstart ndof
    ld.vec v0, Z
    estart
    eend
    st.vec v0, P
    pend
    beq s0, s0 CG_END_BETA //Unconditional jump

    CG_OTHER_ITERATIONS:
    pstart single
    estart
    div.ss s1, s12, s11 //s1 = beta = rho/rhoold
    eend
    pend

    pstart ndof
    ld.vec v0, P
    ld.vec v1, Z
    estart
    mul.sv v0, s1, v0 //v0 = beta*p[i]
    add.vv v0, v0, v1 //v0 = beta*p[i] + z[i]
    eend
    st.vec v0, P
    eend
    pend

    CG_END_BETA:
    { //Apply state operator with xphys and P as inputs, Q as output
        //Setup constant values
        pstart single
        estart
        add.is s1, s0, 0.0078125
        mul.ss s1, s1, s1 // s2 =6.1e-5
        add.is s2, s0, 0.015625
        mul.ss s2, s1, s2 //s2 =9.5e-7 ~= 1e-6 = emin
        add.is s1, s0, 1 //s1=1=E0
        sub.ss s1, s1, s2 // s1 = e0-emin
        mul.sv v0, s0, v0 //Clear v0
        eend
        pend

        //Clear output vector
        pstart ndof
        estart
        eend
        st.vec v0, Q
        pend

        pstart nelemdof //Loop over elements and perform applystateoperator
        ld.dof v0, P
        ld.dof v1, Q
        ld.elem x0, XPHYS
        estart
        mul.xx x1, x0, x0 //x0 = pow(x,2)
        mul.xx x1, x1, x0 //x1 = pow(x,3)
        mul.sx x1, s1, x1 //x1 = pow(x,3)*(e0-emin)
        add.sx x1, s2, x1 //x1 = emin+pow(x,3)*(e0-emin)
        mul.xv v2, x1, v0 //v2 = u_local*elementScale
        mac.kv v2, v2 //ke*(u_local*elementScale)
        add.vv v2, v2, v1 //out[edof[i]] += out_local[i]
        eend
        st.dof v2, Q
        st.fdof v0, Q //Storing fixed DOFs with input values from P
        pend
        //End of apply state operator
    }

    //Dot product of p and q
    pstart ndof
    ld.vec v0, P
    ld.vec v1, Q
    estart
    mac.vv s10, v0, v1 //s10 = dpr(P, Q)
    eend
    pend

    pstart single
    estart
    div.ss s9, s12, s10 //s9 = alpha = rho/dpr
    add.ss s11, s0, s12 //s11 = rhoold = rho
    eend
    pend

    //Update u and r values
    pstart ndof
    ld.vec v0, P
    ld.vec v1, Q
    ld.vec v2, U
    ld.vec v3, R
    estart
    mul.sv v0, s9, v0 //v0 = alpha*p[i]
    mul.sv v1, s9, v1 //v1 = alpha*q[i]
    add.vv v2, v0, v2 //v2 = u[i] + alpha*p[i]
    sub.vv v3, v1, v3 //v3 = r[i] - alpha*q[i]
    eend
    st.vec v2, U
    st.vec v3, R
    pend

    { //Calculate relres
        pstart ndof //Calculate sum(R[i]*R[i])
        ld.vec v0, R
        estart
        mac.iv s1, v0, 1
        eend
        pend

        //Calculate square root of sum to get norm
        pstart single
        estart
        //s1 holds S value
        mul.is s2, s1, 0.5 //s2 = xn = S/2
        add.is s3, s0, 0.5 //s3 = constant 1/2
        //Start looping
        div.ss s4, s1, s2 //s4 = S/xn
        add.ss s4, s4, s2 //s4 = xn + S/xn
        mul.ss s2, s3, s4 //s2 = xnew = 1/2*(xn+S/xn) //First loop
        div.ss s4, s1, s2 //s4 = S/xn
        add.ss s4, s4, s2 //s4 = xn + S/xn
        mul.ss s2, s3, s4 //s2 = xnew = 1/2*(xn+S/xn) //Second loop
        div.ss s4, s1, s2 //s4 = S/xn
        add.ss s4, s4, s2 //s4 = xn + S/xn
        mul.ss s2, s3, s4 //s2 = xnew = 1/2*(xn+S/xn) //Third iteration
        div.ss s4, s1, s2 //s4 = S/xn
        add.ss s4, s4, s2 //s4 = xn + S/xn
        mul.ss s2, s3, s4 //s2 = xnew = 1/2*(xn+S/xn) //Fourth iteration
        div.ss s4, s1, s2 //s4 = S/xn
        add.ss s4, s4, s2 //s4 = xn + S/xn
        mul.ss s2, s3, s4 //s2 = sqrt(S) = norm(r)  //Fifth iteration
        //s2 = sqrt(x1)
        eend
        pend

        pstart single
        estart
        div.ss s2, s2, s13 //s2 = norm(r)/bnorm = relres
        eend
        pend
    }

    bge s2, s6, CGMAINLOOP //if relres>= tol, we're not finished
    //End of solvestatecg

    //Compliance and sensitvity
    //Density filter gradient


}