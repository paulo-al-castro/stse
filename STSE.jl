using Distributions
using Statistics
using CSV
using StringEncodings
using Printf

#MathKurtosis - kurtosis (from Distributions)
#MathSkewness  - skewness (from Distributions)
#Normal Distribution =  
# MathCumulativeDistributionNormal  - cdf (nd,x)


""" 
  returns a series of returns given a serie of prices
""" 
function calcReturnsFromPrice(prices::Array{Float64}) 
   ret=1;
   s=size(prices)[1]
   rreturns=Array{Float64,1}()
   for  i=1:(s-1) 
      push!(rreturns,prices[i+1]/prices[i]-1)
   end
   return rreturns;
end


""" 
  returns a series of prices  given a serie of returns
""" 
function calcPricesFromReturns(rreturns::Array{Float64}) 
   ret=1;
   s=size(rreturns)[1]
   equities=Array{Float64,1}()
   push!(equities,1000000)
   for  i=1:s 
      push!(equities,(rreturns[i]+1)*equities[i])
   end
   return equities;
end

""" 
returns the Total return of a series of returns given of the n first returns

""" 
function calcTotalReturn(returns::Array{Float64}) 
   ret=1;
   s=size(returns)[1]
   for  i=1:s 
      ret*=(1+returns[i])
   end
   return ret-1;
end

""" 
returns the arithmetic average return of the series of returns given of the n first returns

""" 
function calcAvgReturn(returns::Array{Float64}) 
   sum=Float64(0)
   s=size(returns)[1]
   for  i=1:s 
      sum+=returns[i]
   end
   return sum/s;
end

function calcAnnualReturn(returns::Array{Float64}, numberOfDays) 
    gReturn=calcTotalReturn(returns);
   return (1+gReturn)^(252.0/numberOfDays)-1;
end



function calcAnnualSR(returns::Array{Float64}, riskfree, numberOfDays) 
   return sqrt(252)*calcSR(returns,riskfree)
end

""" 
 returns the sample standard deviation
""" 
function calcDesvPad(x::Array{Float64}) 
   avg=calcAvgReturn(x);
   n=size(x)[1];
   s=0;
    for i=1:n
        s+= (x[i]-avg)^2;
    end
  s=s/n;
  return sqrt(s);
end


function calcSR(returns::Array{Float64}, riskfree) 
  avg=calcAvgReturn(returns);
   sigma=calcDesvPad(returns);

    if(sigma!=0)
       return (avg-riskfree)/sigma;
    end
    return -1;
end


""" 
  returns Prob that SR is greater than Threshold
""" 
function ProbSRgreaterThreshold(returns::Array{Float64},threshold) 
   n=size(returns)[1];
   sre=calcSR(returns,0);
   return Z( (sre-threshold)*sqrt(n-1)/
              sqrt(1 - skewness(returns)*sre + (kurtosis(returns)-1)*sre*sre/4)  
            );
end


""" 
 returns the  mininum Track Record(minTRL) lenght (in years) to have returns above threshold wiht level of conficence (alpha)
""" 
function minTRL(returns::Array{Float64},  threshold,  alpha) 
   sre=calcSR(returns,0);
   minT= 1.0+ ( 1.0 -skewness(returns)*sre
         + (kurtosis(returns)-1)*sre*sre/4.0)*
         (Z(alpha)/(sre-threshold))^2.0 ;
   return minT/252         
end

function Z( x) 
   nd=Normal(0,1)
  return cdf(nd,x)
end



"""
  Print Strategy performance STSE report. It is calculated from a equity time series (equities parameter) observed in a given number of days (numberOfDays parameters). 
     Equity series should be an array of float64, from oldest to newest equity value marked to market.
     You should provide at least 30 data points and the number of days can not be smaller than the number of points.
     You may define a desired threshold. The results for zero threshold is always presentes. The default value for threshold is 0.1.
     You may also define a risk free return rate for Sharpe ratio calculation.
"""

function analyzeEquitySeries(equities::Array{Float64},numberOfDays,threshold=0.1,risk_free_rate=0.0)
   n=size(equities)[1]
   if n<30 || numberOfDays>n
      println("ERROR!!")
      println("        You should provide at least 30 data points and the number of days can not be bigger than the number of points")
      println("         ",n," points where provided!!")
      return
   end

   rreturns=calcReturnsFromPrice(equities);
   rreturns=convert(Array{Float64},rreturns);
   rreturns=convert(Array{Float64},rreturns);

   @printf("calcTotalReturn() (%%)=%.2f\n", calcTotalReturn(rreturns)*100);
   @printf("calcAnnualReturn (%%)=%.2f\n",calcAnnualReturn(rreturns,numberOfDays)*100)
   @printf("calcDevPad=%.2f\n",calcDesvPad(rreturns))
   @printf("calcSR=%.2f\n",calcSR(rreturns,risk_free_rate))
   @printf("calcAnnualSR=%.2f\n",calcAnnualSR(rreturns,risk_free_rate,numberOfDays ))
   @printf("ProbSRgreaterThreshold(0)=%.3f\n",ProbSRgreaterThreshold(rreturns,0))
   @printf("minTRL(0)=%.1f\n",minTRL(rreturns,0,0.95))
   @printf("ProbSRgreaterThreshold(%.2f)=%.3f\n",threshold,ProbSRgreaterThreshold(rreturns,0.05))
   @printf("minTRL(%.2f)=%.1f\n",threshold,minTRL(rreturns,0.05,0.95))

end

"""
Print Strategy performance STSE report.

Parameters:
fileName  
numberOfDays 
delim='\t'  

Given a equity file with the following format (UTF16 codification): 

<DATE>	<BALANCE>	<EQUITY>	<DEPOSIT LOAD>
2019.10.01 00:00	100000.00	100000.00	0.0000
2019.10.03 17:59	99962.79	100009.43	0.0022

This kind of files are generated by Metatrader5

Note: The deposit load value shows the percent of account funds used to open positions. Load calculation formula:

Load = Margin / Equity * 100%
https://www.mql5.com/en/articles/2704#load

"""
function processEquityFileUtf16(fileName,numberOfDays,delim='\t')
   s=read(fileName,String,enc"UTF-16");
    print("delim=",delim)
    path = tempname();
    f = open(path, enc"UTF-8", "w");
    write(f, s);
    close(f); # Essential to complete encoding

    cv=CSV.read(path;delim=delim,types=Dict(1=>String,2=>String),normalizenames=true,dateformat="yyyy.mm.dd hh:ss")
 
   equities=convert(Array{Float64},cv[!,3])
 
   analyzeEquitySeries(equities,numberOfDays)
    return cv;
 end
 


"""
Print Strategy performance STSE report.
Parameters:fileName,numberOfDays,delim[opcional]\n

fileName - 
numberOfDays - 
delim=','  

O arquivo apontado em fileName deve estar no formato abaixo:

<DATE>,	<BALANCE>	<EQUITY>	<DEPOSIT LOAD>
2019.10.01 00:00,	100000.00,	100000.00,	0.0000
2019.10.03 17:59,	99962.79,	100009.43,	0.0022

e CODIFICAÇÃO UTF-8 (arquivos gravados pelo Metatrader5 geralmente são UTF16 enquanto gravado pelo python em geral são UTF-8)

Observacao: The deposit load value shows the percent of account funds used to open positions. Load calculation formula:

Load = Margin / Equity * 100%
https://www.mql5.com/en/articles/2704#load

"""
function processEquityFileUtf8(fileName,numberOfDays,delim=',')
  """ s=read(fileName,String,enc"UTF-16");
   print("delim=",delim)
   path = tempname();
   f = open(path, enc"UTF-8", "w");
   write(f, s);
   close(f); # Essential to complete encoding
   """
   cv=CSV.read(fileName;delim=delim,types=Dict(1=>String,2=>String),normalizenames=true,dateformat="yyyy.mm.dd hh:ss")

  # print(head(cv))
  equities=convert(Array{Float64},cv[!,3])

  analyzeEquitySeries(equities,numberOfDays)
  return cv;
end



"""
Print Strategy performance STSE report.
Parameters:fileName,numberOfDays,delim[opcional]\n

fileName - 
numberOfDays - 
delim=','  

The file must be in csv format with the following columns ( UTF-8 codification):
	row_number,date,balance,equity,load,orders

 These file may created by analyze_TestReport.py, using as raw data the XLS file Report created by StrategyTester in MetatTrader5 platform (StrategyTester | tab Backtest | Report (Open XML (MS office Excel)))

Note: The deposit load value shows the percent of account funds used to open positions. Load calculation formula:

Load = Margin / Equity * 100%
https://www.mql5.com/en/articles/2704#load

"""
function processEquityFilePy(fileName,numberOfDays,delim=',')
  """ s=read(fileName,String,enc"UTF-16");
   print("delim=",delim)
   path = tempname();
   f = open(path, enc"UTF-8", "w");
   write(f, s);
   close(f); # Essential to complete encoding
   """
   cv=CSV.read(fileName;delim=delim,types=Dict(1=>String,2=>String),normalizenames=true,dateformat="yyyy.mm.dd hh:ss");

  # print(head(cv))
   equities=convert(Array{Float64},cv[!,4])

     analyzeEquitySeries(equities,numberOfDays);
    return cv;
end



"""
Analisa desempenho de trader usando arquivo com formato abaixo.
Parameters:fileName,numberOfDays,delim[opcional]\n

fileName - 
numberOfDays - 
delim=','  

The file must be in csv format with the following columns ( UTF-8 codification):
	row_number,date,balance,equity,load,orders

O arquivo apontado em fileName deve estar no formato abaixo:

   "counter","date-hour","tickReturn","asset Return","currentBalance"
"""
function processFileRF(fileName, numberOfDays,delim=',')

 cv=CSV.read(fileName;delim=delim,types=Dict(1=>String,2=>String),normalizenames=true,dateformat="yyyy-mm-dd hh:mm:ss")

   rreturns=convert(Array{Float64},cv[!,3])
   rreturns=rreturns.-1 # em formato RF o arquivo usa 1.0 para retorno zero!!!
   equities=calcPricesFromReturns(rreturns)
   println(" ---equities--- ")
   print(equities)
   println(" ---equities end--- ")
   #equities=convert(Array{Float64},cv[!,3])
   analyzeEquitySeries(equities,numberOfDays)
end






"""
Analisa desempenho de trader
Parâmetros:fileName,numberOfDays,delim[opcional]\n

fileName - string com nome do arquivo
numberOfDays - numero de dias de operação equivalente ao periodo observado
delim=';'  - caracter delimitador, por default ;

O arquivo apontado em fileName deve estar no formato abaixo:

   "counter","date-hour","tickReturn","asset Return","currentBalance"
"""
function processFile(fileName,numberOfDays,delim=';')
processFile(fileName,"",numberOfDays,delim);
end

function processFile(fileName,fileName2, numberOfDays,delim)


 #  FileWrite(fileHandle,cont,datahora,StringFormat("%.8f", tickReturn),StringFormat("%.8f", assetReturn),StringFormat("%.8f",currentBalance));

 s=read(fileName,String,enc"UTF-16");
 #e=encode(s,"UTF-8");
 #d=decode(e,"UTF-8");

 path = tempname();
 f = open(path, enc"UTF-8", "w");
 write(f, s);

 close(f); # Essential to complete encoding

if fileName2!=""
   s2=read(fileName,String,enc"UTF-16");
 
   f = open(path, enc"UTF-8", "w");
   write(f, s2);
  
   close(f); # Essential to complete encoding
end


 cv=CSV.read(path;delim=delim,types=Dict(1=>String,2=>String),normalizenames=true,dateformat="yyyy.mm.dd hh:ss")

   rreturns=convert(Array{Float64},cv[!,3])
   equities=calcPricesFromReturns(rreturns)
   #equities=convert(Array{Float64},cv[!,3])
   analyzeEquitySeries(equities,numberOfDays)
end

