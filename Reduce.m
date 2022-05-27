clear 
clc
M1=[114000;8160;8610;0.3;0.3;0.45;4160;4160;3000;1;1;1;1;1;1]%E11,E22,E33,NU12,NU13,N23,G12,G13,G23
R=[0.0001,0.0001,0.0001,0.01,0.01,0.01,0.0001,0.0001,0.0001,1,0,0,0,0,0;%F
    1,0.01,1,0.01,1,0.01,0.1,0.1,1,0,1,0,0,0,0;%M
    1,1,0.01,1,0.01,0.01,0.2,0.2,1,0,0,1,0,0,0;%3
    1,0.01,1,0.01,1,0.01,0.01,1,1,0,0,0,1,0,0;%S12
    1,1,0.01,1,0.01,0.01,1,0.01,1,0,0,0,0,1,0;%S13
    1,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0,0,0,0,0,1]%S23
for i=1:6
    M=M1*R(i,:)
    for j=1:15
    MR(i,j)=M(j,j)
    end
end%算出了每一种基本破坏模式的折减MR
M2=zeros(6,6,64)
M2(6,6,2)=1
M2(5,5,3)=1
A=[0:63]
AA=dec2bin(A,6)
S=str2num(AA)
for k=4:64
     N=floor(log10(S(k)))+1
     if N==2
         x=str2num(num2str(S(k))')'
         l=6-N
         M2(1+l,1+l,k)=x(1)
         M2(2+l,2+l,k)=x(2)
     else if N==3
         x=str2num(num2str(S(k))')'
         l=6-N
         M2(1+l,1+l,k)=x(1)
         M2(2+l,2+l,k)=x(2)
         M2(3+l,3+l,k)=x(3)
         else if N==4
         x=str2num(num2str(S(k))')'
         l=6-N
         M2(1+l,1+l,k)=x(1)
         M2(2+l,2+l,k)=x(2)
         M2(3+l,3+l,k)=x(3)
         M2(4+l,4+l,k)=x(4)
         else if N==5
         x=str2num(num2str(S(k))')'
         l=6-N
         M2(1+l,1+l,k)=x(1)
         M2(2+l,2+l,k)=x(2)
         M2(3+l,3+l,k)=x(3)
         M2(4+l,4+l,k)=x(4)
         M2(5+l,5+l,k)=x(5)
         else if N==6
         x=str2num(num2str(S(k))')'
         l=6-N
         M2(1+l,1+l,k)=x(1)
         M2(2+l,2+l,k)=x(2)
         M2(3+l,3+l,k)=x(3)
         M2(4+l,4+l,k)=x(4)
         M2(5+l,5+l,k)=x(5)
         M2(6+l,6+l,k)=x(6)
             end
             end
             end
         end
     end
     
end   

for m=1:64
MRC(:,:,m)=M2(:,:,m)*MR
end
MRC9=zeros(6,15,64)
for i=1:64;
    for j=1:15;
        for k=1:6;
        if MRC(k,j,i)==0;
            MRC9(k,j,i)=999999999;
        else 
            MRC9(k,j,i)=MRC(k,j,i);
        end
        end
    end
end
RR=zeros(64,15)
for n=1:64
    for p=1:9
        a=min(MRC9(:,p,n))
        RR(n,p)=a
        RR(n,p+1)=sum(MRC(:,p+1,n))
        RR(n,p+2)=sum(MRC(:,p+2,n))
        RR(n,p+3)=sum(MRC(:,p+3,n))
        RR(n,p+4)=sum(MRC(:,p+4,n))
        RR(n,p+5)=sum(MRC(:,p+5,n))
        RR(n,p+6)=sum(MRC(:,p+6,n))
    end
end
RR(1,:)=[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
xlswrite('D:\Matlab-workspace\LW\reduce\Reducee.xlsx',RR)
