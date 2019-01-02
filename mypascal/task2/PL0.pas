program  PL0 ( input, output);
{带有代码生成的PL0编译程序}
//label  99;


{常量定义}
const
  norw = 13; {保留字的个数}
  txmax = 100; {标识符表长度}
  nmax = 14; {数字的最大位数}
  al = 10; {标识符的长度}
  amax = 2047; {最大地址}
  levmax = 3; {程序体嵌套的最大深度}
  cxmax = 200; {代码数组的大小}

{类型变量定义}
type
  symbol = (nul, ident, number, plus, minus, times, slash, oddsym,
  eql, neq, lss, leq, gtr, geq, lparen, rparen, comma, semicolon,
  period, becomes, beginsym, endsym, ifsym, thensym,
  whilesym, dosym, callsym, constsym, varsym, procsym,redsym, wrtsym );
  alfa = packed array [1..al] of char;
  object_ = (constant, variable, procedure_);
  symset = set of symbol;
  fct = (lit, opr, lod, sto, cal, int, jmp, jpc, red, wrt); {functions}
  instruction = packed record
    f : fct;  {功能码}
    l : 0..levmax; {相对层数}
    a : 0..amax; {相对地址}
end;
  {LIT 0,a : 取常数a
  OPR 0,a : 执行运算a
  LOD l,a : 取层差为l的层﹑相对地址为a的变量
  STO l,a : 存到层差为l的层﹑相对地址为a的变量
  CAL l,a : 调用层差为l的过程
  INT 0,a : t寄存器增加a
  JMP 0,a : 转移到指令地址a处
  JPC 0,a : 条件转移到指令地址a处 }

{全局变量定义}
var
  ch : char; {最近读到的字符}
  sym : symbol; {最近读到的符号}
  id : alfa; {最近读到的标识符}
  num : integer; {最近读到的数}
  cc : integer; {当前行的字符计数}
  ll : integer; {当前行的长度}
  kk, err : integer;
  cx : integer; {代码数组的当前下标}
  line : array [1..81] of char;
  a : alfa;
  code : array [0..cxmax] of instruction;
  word : array [1..norw] of alfa;
  wsym : array [1..norw] of symbol;
  ssym : array [char] of symbol;
  mnemonic : array [fct] of packed array [1..5] of char;
  {中间代码算符的字符串}
  declbegsys, statbegsys, facbegsys : symset;
  table : array [0..txmax] of
         record
           name : alfa;
           case kind : object_ of
            constant : (val : integer);
            variable, procedure_ : (level, adr : integer)
         end;

  {新加入}
  fin : text; {输入文件}
  fout: text; {输出文件}


{错误处理程序}         
procedure error (n : integer);
begin 
  writeln(fout,'****', ' ' : cc-1, '^', n : 2);  
  err := err + 1
end {error};

{词法分析程序}
procedure getsym;
  var  i, j, k : integer;

{取下一字符程序}
procedure  getch ;
begin
  if cc = ll then
  begin
    if eof(fin) then
    begin
      write(fout,'PROGRAM INCOMPLETE');
      close(fin);
      close(fout);
      exit;// goto 99;
    end;
    {读新的一行，cx : 5位数}
    ll := 0; cc := 0; write(fout,cx : 5, ' ');
    {如果不是行末}
    while not eoln(fin) do
    begin
      ll := ll + 1; read(fin,ch); write(fout,ch);
      line[ll] := ch
    end;
    //writeln; 
    writeln(fout); {换行}
    readln(fin);{从源文件下一行开始读取}
    ll := ll + 1; 
    line[ll] := ' ' { process end-line }
  end;
  cc := cc + 1; ch := line[cc]
end {getch};

begin {getsym}
  {跳过无用空白}
  while ch = ' ' do getch;
  {标识符或保留字}
  if ch in ['a'..'z'] then
  begin  
    k := 0;
    repeat
      if k < al then
        begin k:= k + 1; a[k] := ch
        end;
      getch
    until not (ch in ['a'..'z', '0'..'9']);
    if k >= kk  then kk := k 
    else
      {如果标识符长度不是最大长度, 后面补空白}
      repeat a[kk] := ' '; kk := kk-1
      until kk = k;
    
    {id中存放当前标识符或保留字的字符串}
    {用二分查找法在保留字表中找当前的标识符id}
    id := a;  i := 1;  j := norw;
    
    repeat  
      k := (i+j) div 2;
      if id <= word[k] then j := k-1;
      if id >= word[k] then i := k + 1
    until i > j;
    
    {如果找到, 当前记号sym为保留字, 否则sym为标识符}
    if i-1 > j then sym := wsym[k] 
    else sym := ident

  end 

  {数字} 
  else if ch in ['0'..'9'] then
  begin
    {当前记号sym为数字}
    k := 0;  num := 0;  sym := number;
    repeat
      {计算数字串的值}
      num := 10*num + (ord(ch)-ord('0'));
      {ord(ch)和ord(0)是ch和0在ASCII码中的序号}
      k := k + 1;  getch;
    until not(ch in ['0'..'9']);
    {当前数字串的长度超过上界,则报告错误}
    if k > nmax then  error(30)
  end 

  {处理赋值号}
  else if ch = ':' then
  begin  
    getch;
    if ch = '=' then
      begin  
        sym := becomes; getch 
      end
    else  
    sym := nul;
  end 
  {新加入}
  {处理>,处理>=}
  else if ch = '>' then
  begin
    getch;

    if ch = '=' then
      begin sym := geq; getch end

    else sym := gtr
  end
  {处理<,处理<=,处理<>}
  else if ch = '<' then
  begin
    getch;

    if ch = '=' then
      begin sym := leq; getch end

    else if ch = '>' then
      begin sym := neq; getch end

    else sym := lss
  end
  {处理其它算符或标点符号}
  else
  begin  
    sym := ssym[ch];  getch
  end
end {getsym};


procedure  gen(x : fct; y, z : integer);
begin
  {如果当前指令序号>代码的最大长度}
  if cx > cxmax then 
    begin write(fout, 'PROGRAM TOO LONG');
    close(fin);
    close(fout);
    exit;// goto 99
    end;
    {在代码数组cx位置生成一条新代码}
    with code[cx] do
    begin  
      f := x;  {功能码} 
      l := y;  {层号}
      a := z   {地址}
    end;
  {指令序号加1}
  cx := cx + 1
end {gen};



procedure  test(s1, s2 : symset; n : integer);
begin
  if not (sym in s1) then
  {如果当前记号不属于集合S1,则报告错误n}
  begin  
    error(n);  
    s1 := s1 + s2;
    {跳过一些记号, 直到当前记号属于S1∪S2}
    while not (sym in s1) do getsym
  end
end {test};




procedure  block(lev, tx : integer; fsys : symset);
var
  dx : integer; {本过程数据空间分配下标}
  tx0 : integer; {本过程标识表起始下标}
  cx0 : integer; {本过程代码起始下标}


procedure  enter(k : object_);
begin {把object填入符号表中}
  tx := tx +1;
  {在符号表中增加新的一个条目}
  with table[tx] do
  begin  
    {当前标识符的名字,种类} 
    name := id;  
    kind := k;
    case k of
      {当前标识符是常数}
      constant : 
        begin
             {当前常数值大于上界,则出错}
            if num > amax then
            begin error(30); num := 0 end;
            val := num
        end;
      {当前标识符是变量}  
      variable : 
        begin
            level := lev;  
            adr := dx;  
            dx := dx +1;
        end;
      procedure_ : 
         {本过程的嵌套层数}
        level := lev
    end
  end
end {enter};



{position返回id在符号表的入口}
function  position(id : alfa) : integer;
var  i : integer;
begin {在标识符表中查标识符id}
  {在符号表栈的最下方预填标识符id} 
  table[0].name := id;  
  {符号表栈顶指针}
  i := tx;
  {从符号表栈顶往下查标识符id}
  while table[i].name <> id do 
    i := i-1;
  position := i {若查到,i为id的入口,否则i=0 }
end {position};



procedure constdeclaration;
begin
  {当前记号是常数}
  if sym = ident then
    begin  getsym;
    {当前记号是等号或赋值号}
    if sym in [eql, becomes] then
    begin
      
      if sym = becomes then 
        error(1);
        getsym;
      
      if sym = number then 
      begin  
        enter(constant); 
        getsym
      end
      {如果=后面不是常数，报错}
      else error(2)
    end 
    {标识符后不是等号和赋值号，报错}
    else error(3)
  end 
  {常数声明中没有常数标识符，报错}
  else error(4)
end {constdeclaration};




procedure  vardeclaration;
begin
  {如果当前记号是标识符}
  if sym = ident then
  {将该变量名加入符号表的下一条目}
  begin  enter(variable);  getsym
  end 
  else error(4)
end {vardeclaration};



procedure  listcode;
var  i : integer;
begin  {列出本程序体生成的代码}
  {cx0: 本过程第一个代码的序号, 
  cx-1: 本过程最后一个代码的序号}
  for i := cx0 to cx-1 do
  with code[i] do
    writeln(fout,i, mnemonic[f] : 5, l : 3, a : 5)
    {i: 代码序号; 
        mnemonic[f]: 功能码的字符串;
        l: 相对层号(层差);
        a: 相对地址或运算号码}
end {listcode};




procedure  statement(fsys : symset);
var  i, cx1, cx2 : integer;

procedure  expression(fsys : symset);
var  addop : symbol;

procedure  term(fsys : symset);
var  mulop : symbol;


procedure  factor(fsys : symset);
var i : integer;
begin  
  {测试当前的记号是否因子的开始符号, 
  否则出错, 跳过一些记号}
  test(facbegsys, fsys, 24);
  {如果当前的记号是否因子的开始符号}
  while sym in facbegsys do
  begin
        {当前记号是标识符}
        if sym = ident then
        begin
          i := position(id);
          if i = 0 then error(11) 
          else
            {若在符号表中查不到id, 则出错,
             否则,做以下工作}
            with table[i] do
            case kind of
              constant : gen(lit, 0, val);
              variable : gen(lod, lev-level, adr);
              procedure_ : error(21)
            end;
          getsym
        end
        {当前记号是数字}
        else if sym = number then
        begin
          if num > amax then
          {若数值越界,则出错}
          begin error(30); num := 0 end;
          gen(lit, 0, num); getsym
        end 
        {如果当前记号是左括号}
        else if sym = lparen then
        begin  getsym;
          expression([rparen]+fsys);
          if sym = rparen then getsym
          else error(22)
        end;
        {测试当前记号是否同步, 否则出错, 跳过一些记号}
        test(fsys, [lparen], 23)
  end {while}
end {factor};



begin {term}
  {处理项中第一个因子}
    factor(fsys+[times, slash]);
    while sym in [times, slash] do
    {当前记号是*或/号}
    begin
      {运算符存入mulop}
      mulop := sym;  getsym;
      factor(fsys+[times, slash]);
      {生成一条*/指令}
      if mulop = times then gen(opr, 0, 4)
      else gen(opr, 0, 5)
    end
end {term};

begin {expression}
  {若第一个记号是+或-}
  if sym in [plus, minus] then
  begin 
    addop := sym;  
    getsym;
    {处理一个项}
    term(fsys+[plus, minus]);
    if addop = minus then gen(opr, 0, 1)
  end 
  {第一个记号不是+或-, 则处理一个项}
  else term(fsys+[plus, minus]);
  {若当前记号是加号或减号}
  while sym in [plus, minus] do
  begin
    addop := sym;  getsym;
    term(fsys+[plus, minus]);
    if addop = plus then gen(opr, 0, 2)
    else gen(opr, 0, 3)
  end
end {expression};




procedure  condition(fsys : symset);
var  relop : symbol;
begin
  {当前记号是odd}
  if sym = oddsym then 
  begin
  {生成指令,判定表达式的值是否为奇数,
  是,则取“真”;不是, 则取“假”}
    getsym;  expression(fsys);  gen(opr, 0, 6)
  end 
  {当前记号不是odd}
  else
  begin
    expression([eql, neq, lss, gtr, leq, geq] + fsys);
    {如果当前记号不是关系符,报错}
    if not (sym in [eql, neq, lss, leq, gtr, geq]) then
      error(20)  
    else
    begin
      relop := sym;  
      getsym;  
      {处理关系符右边的算术表达式}
      expression(fsys);
      case relop of
        eql : gen(opr, 0, 8);{=}
        neq : gen(opr, 0, 9);{<>}
        lss : gen(opr, 0, 10);{<}
        geq : gen(opr, 0, 11);{>=}
        gtr : gen(opr, 0, 12);{>}
        leq : gen(opr, 0, 13);{<=}
      end
    end
  end
end {condition};


begin {statement}
   {处理赋值语句}
  if sym = ident then 
  begin  
    {在符号表中查id, 返回id在符号表中的入口}
    i := position(id);
    {查不到报错}
    if i = 0 then error(11) 
    else if table[i].kind <> variable then
    begin {对非变量赋值} 
      error(12); 
      i := 0; 
    end;
    getsym;
    {若当前是赋值号, 取下一记号, 否则报错}
    if sym = becomes then getsym 
    else error(13);
    expression(fsys);
    {若赋值号左边的变量id有定义}
    if i <> 0 then
      with table[i] do 
        gen(sto, lev-level, adr)
        {生成一条存数指令, 将栈顶(表达式)的值存入变量id中;
         lev: 当前语句所在过程的层号;
         level: 定义变量id的过程的层号;
         adr: 变量id在其过程的数据空间的相对地址}
  end 

  {处理过程调用语句}
  else if sym = callsym then
  begin  
    getsym;
    {如果下一记号不是过程标识符，报错，是，继续}
    if sym <> ident then error(14) else
    begin 
      {查符号表,返回id位置}
      i := position(id);
      {如果在符号表中查不到, 则报错}
      if i = 0 then error(11) else
        with table[i] do
          {如果在符号表中id的过程名一致，调用，不一致，报错}
          if kind = procedure_ then 
            gen(cal, lev-level, adr)
          else error(15);
          getsym
    end
  end 

  {处理条件if语句}
  else if sym = ifsym then
  begin
    getsym;  
    {如果当前记号是then,则取下一记号}
    condition([thensym, dosym]+fsys);
    if sym = thensym then getsym else error(16);
    {cx1记录下一代码的地址}
    cx1 := cx;  
    {生成指令,表达式为假,转到某地址(待填),否则顺序执行}
    gen(jpc, 0, 0);
    {处理一个语句}
    statement(fsys);  
    {将下一个指令的地址回填到上面的jpc指令地址栏}
    code[cx1].a := cx
  end 


  {处理语句序列}
  else if sym = beginsym then
  begin
    getsym;  
    {处理第一个语句}
    statement([semicolon, endsym]+fsys);
    {如果当前记号是分号或语句的开始符号}
    while sym in [semicolon]+statbegsys do
    begin
      if sym = semicolon then getsym else error(10);
      statement([semicolon, endsym]+fsys)
    end;
    if sym = endsym then getsym else error(17)
  end 

  {处理循环语句}
  else if sym = whilesym then
  begin
    cx1 := cx;  
    getsym;  
    condition([dosym]+fsys);
    cx2 := cx;
    {生成一条指令,表达式为假
    转到某地址(待回填), 否则顺序执行}  
    gen(jpc, 0, 0);
    {处理do语句}
    if sym = dosym then getsym else error(18);
    statement(fsys); 
    { jump到while后的条件表达式的代码的第一条指令处}  
    gen(jmp, 0, cx1);  
    {回填jpc指令地址栏}
    code[cx2].a := cx
  end

  {新加入,未完待续}
  {处理read语句}
  else if sym = redsym then
  begin
    getsym;

    if sym = lparen then
    begin
      getsym;
      {标识符}
      if sym = ident then
      begin
        i := position(id);
        
        if i <> 0 then
        begin
          {类型不是变量}
          if table[i].kind <> variable then
          begin error(12);i := 0 end

          else with table[i] do 
            gen(red, lev-level, adr)

        end
        {表中没有这个标识符,i=0}
        else error(11)
      end
      {不是标识符，报错}
      else error(4);
      getsym
    end
    {不是(，报错}
    else error(40);
    

    getsym
  end

  {处理write语句}
  else if sym = wrtsym then
  begin
    getsym;
    if sym = lparen then
      begin
        getsym;
        expression([rparen, comma]+fsys);
        gen(wrt, 0, 0);
        getsym;
        
      end
    {不是(，报错}
    else error(40);
    
  end;

   {测试下一记号是否正常, 
   否则出错, 跳过一些记号}
  test(fsys, [ ], 19)
end {statement};


begin {block}
  {栈顶指针}
  dx := 3;  
  {当前标识符表的长度}  
  tx0 := tx; 
  {本过程名的地址, 即下一条指令的序号}
  table[tx].adr := cx; 
  gen(jmp, 0, 0);
  {如果当前过程层号>最大层数, 报错}
  if lev > levmax then error(32);
  repeat
    {处理常数说明语句}
    if sym = constsym then 
    begin  
      getsym;
      repeat 
        constdeclaration;
        {如果当前记号是逗号}
        while sym = comma do
          begin 
            getsym; 
            constdeclaration 
          end;
        {如果当前记号是分号,
        则常数说明已处理完, 否则报错}
        if sym = semicolon then getsym else error(5)
      {直到当前记号不是标识符}
      until sym <> ident
    end;
    
    {当前记号是变量说明语句开始符号}
    if sym = varsym then
    begin  getsym;
      repeat 
        vardeclaration;
        while sym = comma do
          begin  getsym;  vardeclaration  end;
        if sym = semicolon then getsym else error(5)
      until sym <> ident;
    end;


    {处理过程说明}
    while sym = procsym do
    begin  getsym;
      if sym = ident then
      {把过程名填入符号表}
      begin  
        enter(procedure_);  
        getsym  
      end
      else error(4);
      {过程名后漏掉分号出错}
      if sym = semicolon then getsym else error(5);
       {lev+1: 过程嵌套层数加1; 
        tx: 符号表当前栈顶指针,也是新过程符号表起始位置;
        [semicolon]+fsys: 过程体开始和末尾符号集}
      block(lev+1, tx, [semicolon]+fsys);
      {如果当前记号是分号}
      if sym = semicolon then
      begin  
        getsym;
        {测试当前记号是否语句开始符号或过程说明开始符号,
         否则报告错误6, 并跳过一些记号}
        test(statbegsys+[ident, procsym], fsys, 6)
      end
      else error(5)
    end;{while}
    {检测当前记号是否语句开始符号, 
    否则出错, 并跳过一些记号}
    test(statbegsys+[ident], declbegsys, 7)
  {直到当前记号不是说明语句的开始符号}
  until not (sym in declbegsys);
  //对应641
   {table[tx0].adr是本过程的第1条
    代码(jmp, 0, 0)的地址,本语句即是将下一代码(本过程语句的第
    1条代码)的地址回填到该jmp指令中,得(jmp, 0, cx)}
  code[table[tx0].adr].a := cx;
  {本过程名的第1条代码的地址改为下一指令地址cx}
  with table[tx0] do
  begin  
    adr := cx; {代码开始地址}
  end;
  cx0 := cx; 
  gen(int, 0, dx);
  statement([semicolon, endsym]+fsys);
  gen(opr, 0, 0); {生成返回指令}
  test(fsys, [ ], 8);
  listcode;
end  {block};



procedure  interpret;
const  stacksize = 500; {上界}
var  p, b, t : integer; {程序地址寄存器, 基地址寄存器,栈顶地址寄存器}
     i : instruction; {指令寄存器}
     s : array [1..stacksize] of integer; {数据存储栈}

//未完待续
function  base(l : integer) : integer;
var  b1 : integer;
begin
  b1 := b; {顺静态链求层差为l的层的基地址}
  while l > 0 do
  begin  
    b1 := s[b1];  
    l := l-1 
  end;
  base := b1
end {base};

begin  
  writeln(fout,'START PL/0');
  {栈顶地址，基地址，程序地址寄存器}
  t := 0;  b := 1;  p := 0;
  {每个过程运行时的数据空间的前三个单元是:SL, DL, RA;
    SL: 指向本过程静态直接外层过程的SL单元;
    DL: 指向调用本过程的过程的最新数据空间的第一个单元;
    RA: 返回地址 }
  s[1] := 0;  s[2] := 0;  s[3] := 0;

  repeat
    i := code[p];  
    p := p+1;
    with i do
      case f of
        lit : 
          begin
            t := t+1;  s[t] := a
          end;
        opr : 
        case a of {运算}
          0 : 
          begin {返回调用过程指令}
            t := b-1;  p := s[t+3];  b := s[t+2];
          end;
          1 : s[t] := -s[t];{-1}
          2 : {+}
          begin
              t := t-1;  s[t] := s[t] + s[t+1]
          end;
          3 : {-}
          begin
              t := t-1;  s[t] := s[t]-s[t+1]
          end;
          4 : {*}
          begin
              t := t-1;  s[t] := s[t] * s[t+1]
          end;
          5 : {int/}
          begin
              t := t-1;  s[t] := s[t] div s[t+1]
          end;
          {算s[t]是否奇数, 是则s[t]=1, 否则s[t]=0}
          6 : s[t] := ord(odd(s[t]));
          {判两个表达式的值是否相等,是则s[t]=1, 否则s[t]=0}
          8 : 
          begin  t := t-1;
              s[t] := ord(s[t] = s[t+1])
          end;
          {判两个表达式的值是否不等,是则s[t]=1, 否则s[t]=0}
          9: 
          begin  t := t-1;
              s[t] := ord(s[t] <> s[t+1])
          end;
          {判前一表达式是否小于后一表达式,是则s[t]=1, 否则s[t]=0}
          10 : 
          begin  t := t-1;
              s[t] := ord(s[t] < s[t+1])
          end;
          {判前一表达式是否大于或等于后一表达式, 是则s[t]=1, 否则s[t]=0}
          11: 
          begin  t := t-1;
              s[t] := ord(s[t] >= s[t+1])
          end;
          {判前一表达式是否大于后一表达式, 是则s[t]=1, 否则s[t]=0}
          12 : 
          begin  t := t-1;
              s[t] := ord(s[t] > s[t+1])
          end;
          {判前一表达式是否小于或等于后一表达式, 是则s[t]=1, 否则s[t]=0}
          13 : 
          begin  t := t-1;
              s[t] := ord(s[t] <= s[t+1])
          end;
        end;
      lod : begin  {当前指令是取变量指令(lod, l, a)}
            t := t + 1;  s[t] := s[base(l) + a]
           end;
      sto : begin  {当前指令是保存变量值(sto, l, a)指令}
            s[base(l) + a] := s[t];  //writeln(fout,s[t]);
            t := t-1
          end;
      cal : begin {generate new block mark}
            s[t+1] := base( l );  
            s[t+2] := b;
            s[t+3] := p;
            b := t+1;  
            p := a
          end;
      int : t := t + a;
      jmp : p := a;
      jpc : begin
            if s[t] = 0 then p := a;
            t := t-1
          end;
      red :
    begin
         writeln('input: ');
         readln(s[base(l)+a]);{读一行数据,读入到相差l层,
         层内偏移为a的数据栈中的数据的信息}
         write(fout,'input: ');
         writeln(fout, s[base(l)+a]);
    end;
wrt : 
    begin
        writeln('output: ');
        writeln(s[t]);
        
        write(fout,'output: ');
        writeln(fout, s[t]); {输出栈顶的信息}
        t := t + 1 {栈顶上移}
    end

    end {with, case}
  until p = 0;
  write(fout,'END PL/0');
end {interpret};



//未完待续
begin  {主程序}

  {新加入}
  {将文件名赋值给fin,fout}
  assign(fin,'in.pas');
  assign(fout,'out.txt'); 
  {打开2个文件}
  reset(fin);
  rewrite(fout);  
  for ch := 'A' to ';' do  ssym[ch] := nul;
  word[1] := 'begin        ';
  word[2] := 'call         ';
  word[3] := 'const        ';
  word[4] := 'do           ';
  word[5] := 'end          ';
  word[6] := 'if           ';
  word[7] := 'odd          ';
  word[8] := 'procedure    ';
  word[9] := 'read         ';
  word[10] := 'then         ';
  word[11] := 'var          ';
  word[12] := 'while        ';
  word[13] := 'write        ';


  wsym[1] := beginsym;
  wsym[2] := callsym;
  wsym[3] := constsym;
  wsym[4] := dosym;
  wsym[5] := endsym;
  wsym[6] := ifsym;
  wsym[7] := oddsym;
  wsym[8] := procsym;
  wsym[9] :=  redsym;
  wsym[10] := thensym;
  wsym[11] := varsym;
  wsym[12] := whilesym;
  wsym[13] := wrtsym;


  ssym['+'] := plus;      
  ssym['-'] := minus;
  ssym['*'] := times;     
  ssym['/'] := slash;
  ssym['('] := lparen;     
  ssym[')'] := rparen;
  ssym['='] := eql;       
  ssym[','] := comma;
  ssym['.'] := period;     
  //ssym[‘≠’] := neq; 不存在
  ssym['<'] := lss;       
  ssym['>'] := gtr;
  //ssym[‘≤’] := leq;      
  //ssym[‘≥’] := geq;
  ssym[';'] := semicolon;

  {中间代码指令的字符串}
  mnemonic[lit] := 'LIT';
  mnemonic[opr] := 'OPR';
  mnemonic[lod] := 'LOD';
  mnemonic[sto] := 'STO';
  mnemonic[cal] := 'CAL';
  mnemonic[int] := 'INT';
  mnemonic[jmp] := 'JMP';
  mnemonic[jpc] := 'JPC';
  mnemonic[red] := 'RED';
  mnemonic[wrt] := 'WRT';

  declbegsys := [constsym, varsym, procsym];
  statbegsys := [beginsym, callsym, ifsym, whilesym, wrtsym, redsym];
  facbegsys := [ident, number, lparen];
  //page(output); 

  err := 0; {发现错误的个数}
  cc := 0;  {当前行中输入字符的指针} 
  cx := 0;  {代码数组的当前指针} 
  ll := 0;  {输入当前行的长度} 
  ch := ' ';  {当前输入的字符}
  kk := al;  {标识符的长度}

  getsym;

  block(0, 0, [period]+declbegsys+statbegsys);
  if sym <> period then error(9);
  if err = 0 then interpret
  else write(fout,'ERRORS IN PL/0 PROGRAM');
  //99 : writeln
  {新加入}
  writeln;
  close(fin);
  close(fout);
end.



