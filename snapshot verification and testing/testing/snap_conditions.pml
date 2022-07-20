proctype testInterval_i(){
	byte proc,x,x_0,x_1,k;
	
q0:	do 
	::startScan?proc-> skip
	::endScan?proc,x_0,x_1 ->
		assert(x_0==0)
	:: atomic {startUpdate?proc,x->
		if
		:: proc==pi && x==1 -> goto q1
		:: else -> skip
		fi}
	::endUpdate?proc,x -> skip
	od;
	
q1:	do 
	::startScan?proc-> skip
	::endScan?proc,x_0,x_1 -> skip
	::startUpdate?proc,x-> skip
	:: atomic {endUpdate?proc,x ->
		if
		:: proc==pi && x==1 -> goto q2
		:: else -> skip
		fi}
	od;	
	
q2:	do 
	:: atomic {startScan?proc-> 
		if
		::skip 
		::k=proc;goto q3 
		fi}
	::endScan?proc,x_0,x_1 -> skip
	::startUpdate?proc,x-> skip
	::endUpdate?proc,x -> skip
	od;	
	
q3:	do 
	::startScan?proc-> skip
	:: atomic {endScan?proc,x_0,x_1 -> 
		if 
		:: proc==k ->assert(x_0==1) 
		::else skip 
		fi}
	::startUpdate?proc,x-> skip
	::endUpdate?proc,x -> skip
	od			
}
	
proctype testInterval_j(){
	byte proc,x,x_0,x_1,k;
	

q0:	do 
	::startScan?proc-> skip
	::endScan?proc,x_0,x_1 ->
		assert(x_1==0)
	:: atomic {startUpdate?proc,x->
		if
		:: proc==pj && x==1 -> goto q1
		:: else -> skip
		fi}
	::endUpdate?proc,x -> skip
	od;
	
q1:	do 
	::startScan?proc-> skip
	::endScan?proc,x_0,x_1 -> skip
	::startUpdate?proc,x-> skip
	:: atomic {endUpdate?proc,x ->
		if
		:: proc==pj && x==1 -> goto q2
		:: else -> skip
		fi}
	od;	
	
q2:	do 
	:: atomic {startScan?proc-> 
		if
		::skip 
		::k=proc;goto q3 
		fi}
	::endScan?proc,x_0,x_1 -> skip
	::startUpdate?proc,x-> skip
	::endUpdate?proc,x -> skip
	od;	
	
q3:	do 
	::startScan?proc-> skip
	:: atomic {endScan?proc,x_0,x_1 -> 
		if 
		:: proc==k ->assert(x_1==1) 
		::else skip 
		fi}
	::startUpdate?proc,x-> skip
	::endUpdate?proc,x -> skip
	od			
}
		
proctype testAdmissiblePair(){
	byte proc,x,x_0,x_1,k;
	

q0:	do 
	::startScan?proc-> skip
	::endScan?proc,x_0,x_1 -> skip
	:: atomic {startUpdate?proc,x->
		if
		:: proc==pi && x==1 -> goto q1
		:: proc==pj && x==1 -> goto q2
		:: else -> skip
		fi}
	::endUpdate?proc,x -> skip
	od;
q1:	do 
	::startScan?proc-> skip
	::endScan?proc,x_0,x_1 -> skip
	:: atomic {startUpdate?proc,x->
		if
		:: proc==pj && x==1 -> goto q5
		:: else -> skip
		fi}
	:: atomic {endUpdate?proc,x -> 
		if
		:: proc==pi && x==1 -> goto q3
		:: else -> skip
		fi}
	od;

q2:	do 
	::startScan?proc-> skip
	::endScan?proc,x_0,x_1 -> skip
	:: atomic {startUpdate?proc,x->
		if
		:: proc==pi && x==1 -> goto q5
		:: else -> skip
		fi}
	:: atomic {endUpdate?proc,x -> 
		if
		:: proc==pj && x==1 -> goto q4
		:: else -> skip
		fi}
	od;

q3:	do 
	::startScan?proc-> skip
	::endScan?proc,x_0,x_1 -> assert(x_0 >= x_1)
	::startUpdate?proc,x-> skip
	::endUpdate?proc,x -> skip
	od;
	
q4:	do 
	::startScan?proc-> skip
	::endScan?proc,x_0,x_1 -> assert(x_1 >= x_0)
	::startUpdate?proc,x-> skip
	::endUpdate?proc,x -> skip
	od;
q5:
}

proctype testProperty1(){
	byte x,x_0,x_1,proc;
	
q0:	do
	::atomic{
		endScan?proc,x_0,x_1 ->
			if 
			:: x_0==0 && x_1==1-> goto q1 
			:: x_0==1 && x_1==0-> goto q2 
			fi
		}
	od;
	
q1:	do
	::atomic {
		endScan?proc,x_0,x_1 ->
			assert(x_0!=1 || x_1!=0)
		}
	od;
	
q2:	do
	::atomic{
		endScan?proc,x_0,x_1 ->
			assert(x_0!=0 || x_1!=1)
		}
	od
}


proctype testProperty2(){
	
	byte proc,x,x_0,x_1;
	byte k;
q0:	
	do
	::atomic{endScan?proc,x_0,x_1 -> 
			if
			:: x_0==1 || x_1==1 -> goto q1
			:: x_0==1 && x_1==1 -> goto q2
			fi
		}
	od;
	
q1: do
	::atomic{startScan?proc-> 	
			if
			::k=proc;goto q3 
			fi 
		}
	od;
	
q2:	 do
	:: atomic{startScan?proc -> 
			if
			::k=proc;goto q4 
			fi
		}
	od;
	
q3:	do
	:: atomic{endScan?proc,x_0,x_1 ->
			if 
			:: proc==k -> assert(x_0!=0 || x_1!=0)
			fi
		}
	od;


q4:	do
	:: atomic{endScan?proc,x_0,x_1 -> 
			if 
			:: proc==k ->assert(x_0 ==1 && x_1==1) 
			::else skip 
			fi
		}
	od;
q5:
}

proctype testProperty3(){
	if
	:: run testInterval_i();
	:: run testInterval_j();
	:: run testAdmissiblePair();
	fi
}


proctype listener(){
	byte proc,x_0,x_1,x;
	do
	:: startUpdate?proc,x-> skip;
	:: endUpdate?proc,x -> skip;
	:: startScan?proc -> skip;
	:: endScan?proc,x_0,x_1 -> skip;
	od
}

active proctype TEST(){		
	if
	:: run testProperty1();run listener()
	:: run testProperty2();run listener()
	:: run testProperty3()
	fi;
} 

