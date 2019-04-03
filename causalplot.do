program causalplot
syntax anything [,TIPO(string)]

local dir `c(pwd')

cd `dir'

local obs `1'
local cal `2'

if ("`tipo'"=="omitida"){
qui{

kplot1 `obs' , tipo(omitida)

markstat using diap_omitida.stmd,slides 
}
}

if ("`tipo'"=="psm"){
qui{

kplot1 `obs' `cal' , tipo(psm)

markstat using diap_psm.stmd,slides 
}
}

if ("`tipo'"=="dd"){
qui{

kplot1 `obs' , tipo(dd)

markstat using diap_dd.stmd,slides 
}
}

if ("`tipo'"=="rd"){
qui{

kplot1 `obs' `cal' , tipo(rd)

markstat using diap_rd.stmd,slides 
}
}

end
