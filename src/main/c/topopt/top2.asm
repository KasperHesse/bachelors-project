//This file contains much of the same content as top.asm, but contains the instructions in the correct order for execution

TOP3DCGSETUP: //This is where all the setup in top3dcg is performed

//populate design space
//Volfrac = 0.2
pstart nelemvec
estart
mul.sv v0, s0, v0
mul.sv v1, s0, v1
add.iv v0, v0, 0.2
add.iv v1, v1, 1
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
{
    //Main function performing the majority of computations. Works on xphys, F and U

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
    mul.xv v2, v1, v0 //v2 = u_local*elementScale
    mac.kv v2, v2 //ke*(u_local*elementScale)
    add.vv v2, v2, v1 //out[edof[i]] += out_local[i]
    eend
    st.dof v2, R
    st.fdof v0, R //Storing fixed DOFs with input values from U
    pend
    //End of apply state operator

    //Calculate value of r[i]=b[i]-r[i], b=F
}