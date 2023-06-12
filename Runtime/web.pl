%:- module(upload, [run/0]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_multipart_plugin)).
:- use_module(library(http/http_client)).
:- use_module(library(http/html_write)).
:- use_module(library(option)).
:- use_module(library(http/http_parameters)).

:- http_handler(root(.),	upload_form, []).
:- http_handler(root(upload),	upload,      []).
:- http_handler(root(draw),	draw,      []).
:- http_handler(root('jquery1.js'), http_reply_file('jquery1.js', []), []). 
:- http_handler(root('jquery_plantuml.js'), http_reply_file('jquery_plantuml.js', []), []). 
:- http_handler(root('rawdeflate.js'), http_reply_file('rawdeflate.js', []), []).
:- http_handler(root('basic.css'), http_reply_file('basic.css', []), []).
:- http_handler(root('c3.css'), http_reply_file('c3.css', []), []).
:- http_handler(root('d3.v3.min.js'), http_reply_file('d3.v3.min.js', []), []).
:- http_handler(root('c3.min.js'), http_reply_file('d3.v3.min.js', []), []).
run :-
	http_server(http_dispatch, [port(8080)]).

upload_form(_Request) :-
    get_template(X),
	format('Content-type: text/html~n~n'),
	format(X, [[],[],[],[],'',[]]).

upload(Request) :-
	multipart_post_request(Request), !,
	http_read_data(Request, Parts,
		       [ on_filename(save_file)
		       ]),
	memberchk(file=file(_, Saved), Parts),
	getobject(Saved,Obj),format_list(Obj,List),
	
	get_template(X),
	format('Content-type: text/html~n~n'),
	format(X, [[],[],List,Saved,'',[]]).

upload(_Request) :-
	throw(http_reply(bad_request(bad_file_upload))).

	
draw(Request) :-  
      http_parameters(Request,
                        [ selVar(Module, [])
                        ,file(Saved,[])]),getobject(Saved,Obj),
                        format_list(Obj,List),
                        diagram(Saved,Module,Res1,Res2,Line)
                        ,get_template(X),
	format('Content-type: text/html~n~n'),
	format(X, [Res1,Res2,List,Saved,Module,Line]).

input_list_concat([],'').
input_list_concat([X],X).
input_list_concat([X|T],Out):-
   input_list_concat(T,Out2),atom_concat(X,',',Out1),
   atom_concat(Out1,Out2,Out).

multipart_post_request(Request) :-
	memberchk(method(post), Request),
	memberchk(content_type(ContentType), Request),
	http_parse_header_value(
    content_type, ContentType,
    media(multipart/'form-data', _)).

:- public save_file/3.

save_file(In, file(FileName, File), Options) :-
   option(filename(FileName), Options),
   setup_call_cleanup(
   tmp_file_stream(octet, File, Out),
   copy_stream_data(In, Out),
   close(Out)).
format_list([],'').
format_list([X|T],List) :- 
   string_concat('<option value="',X,L1),
   string_concat(L1,'">', L2),string_concat(L2,X,L3),
   string_concat(L3,"</option>",L4),format_list(T,L5),
   string_concat(L4,L5,List).
:- multifile prolog:message//1.

prolog:message(bad_file_upload) -->
	[ 'A file upload must be submitted as multipart/form-data using', nl,
	  'name=file and providing a file-name'
	].
	
%----template
get_template('<html> <head>
<script type="text/javascript" src="jquery1.js"></script>
<script type="text/javascript" src="jquery_plantuml.js"></script>
<link rel="stylesheet" type="text/css" href="https://cdnjs.cloudflare.com/ajax/libs/c3/0.4.11/c3.min.css">  
<script src="http://d3js.org/d3.v3.min.js" charset="utf-8"></script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/c3/0.4.11/c3.min.js"></script>
<link rel="stylesheet" type="text/css" href="basic.css">
<script>
function getVar(){
var vSkill = document.getElementById(\'newSkill\');
var vSkillText = vSkill.options[vSkill.selectedIndex].value;
 var lblValue = document.getElementById("selVar").value;
 if(lblValue){
 document.getElementById("selVar").value=lblValue +\':\'+vSkillText;
 }
 else{
 document.getElementById("selVar").value=vSkillText;
 }

return;
}
function changeGraph(){
var gType = document.getElementById(\'graphtype\');

if(gType.selectedIndex>0&&gType.selectedIndex<3){
   document.getElementById("prev").disabled=false;
   document.getElementById("next").disabled=false;
   document.getElementById("comp").disabled=false;
   document.getElementById("chart").style.display="none";
   document.getElementById("state").style.display="block";
}
else{

   document.getElementById("prev").disabled=true;
   document.getElementById("next").disabled=true;
   document.getElementById("comp").disabled=true;
   document.getElementById("chart").style.display="block";
   document.getElementById("state").style.display="none";
}

if(gType.selectedIndex==1){
getTimeDiagram();
}
if(gType.selectedIndex==2){
    getAbstractDiagram();
}

}
var timestate=~w;
var abstractstate=~w;
var timeindex=1;
var absindex=1;

function nextState(){
var gType = document.getElementById(\'graphtype\');
if(gType.selectedIndex==1){
 if(timeindex<timestate.length)
        timeindex =timeindex+1;
    getTimeDiagram();
}
if(gType.selectedIndex==2){
 if(absindex<abstractstate.length)
        absindex =absindex+1;
    getAbstractDiagram();
}

}

function previousState(){
var gType = document.getElementById(\'graphtype\');
   if(gType.selectedIndex==1){
 if(timeindex>0)
        timeindex =timeindex-1;
    getTimeDiagram();
}
if(gType.selectedIndex==2){
 if(absindex>0)
        absindex =absindex-1;
    getAbstractDiagram();
}
}

function fullStates(){
var gType = document.getElementById(\'graphtype\');
    if(gType.selectedIndex==1){
 
timeindex =timestate.length;
    getTimeDiagram();
}
if(gType.selectedIndex==2){
 
        absindex =abstractstate.length;
    getAbstractDiagram();
}
}

function getTimeDiagram(){

if(timestate){
var dia=\'\';
var prev=\'[*]\';
for (var i = 0; i < timeindex; i++) {
   var step = timestate[i];
   dia = (dia+ step[0]+step[1]+step[2]+"\\n"+ step[2]+step[3]+step[4]+"\\n");
}
}
document.getElementById("grapharea").innerHTML="<img uml=\'"+ dia + "\'>";

console.log(\'<img uml="\'+ dia + \'>\');
return;
}

function getAbstractDiagram(){

if(abstractstate){
var dia=\'\';

for (var i = 0; i < absindex; i++) {
   var step = abstractstate[i];
   dia = (dia+ step[0]+step[1]+step[2]+"\\n");
   

}
}
document.getElementById("grapharea").innerHTML="<img uml=\'"+ dia + "\'>";

console.log("<img uml=\'"+ dia + "\'>");
return;
}

</script>

</head><body>
<h1 style="color: #5e9ca0; ">&nbsp;TCOB Trace Visualization</h1>

<ul class="form-style-1">
<form action="/upload" enctype="multipart/form-data" method="POST">
    <li><label>Debug File<span class="required">*</span></label>
    <input name="file" type="file" class="field-divided" required/>&nbsp;<input type="submit" value="Upload!" />
    </form>
    
        <label>Variable Set</label>
        <select id="newSkill" name="varlist" class="field-select">
~w
</select>&nbsp;<input type="submit" onclick="getVar()" value="Add"/>
    </li>
    <form action="/draw" method="GET">
    <input type=hidden name=file value="~w" />
    <li>
        <label>Selected Variable<span class="required">*</span></label>
        <textarea name="selVar" id="selVar" id="field5" class="field-long field-textarea" required>~w</textarea>
    </li>
    <li>
     <input type="submit" value="Draw Diagram" />   
    </li>
    </form>
<div id="graphdiv">
<li>
 <select id="graphtype" name="graphtype" class="field-select" onchange="changeGraph()"> 
 <option value="select">Select Graph Type..</option>
<option value="timeg">Time Graph</option>
<option value="absg">Abstract Graph</option>
<option value="absg">Line Graph</option>
</select> &nbsp; <input type="submit" value="Previous" onclick="previousState()" id="prev"/> &nbsp; 
<input type="submit" value="Next" onclick="nextState()" id="next"/> 
<input type="submit" value="Complete Diagram" onclick="fullStates()" id="comp"/>
</li> 

<li><div id="graph" class="ScrollStyle"><div id="state"> 
<p id="grapharea" class="grapharea"></p></div></li> <div id="chart"></div></div> </div>
</ul>
<script>


if(timestate.length==0){
document.getElementById("graphdiv").style.display="none";
}
else{

document.getElementById("graphdiv").style.display="block";
}
document.getElementById("chart").style.display="none";
 try{
var chart = c3.generate({
    bindto: \'#chart\',
    data: {
       x: \'x\',
      columns: ~w
    }
});}
catch(err){
document.getElementById("chart").innerHTML="Non-numeric input or error";
}

</script>
</body></html>').
