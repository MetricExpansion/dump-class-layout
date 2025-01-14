; NOTE: Assertions have been autogenerated by utils/update_test_checks.py
; RUN: opt -slp-vectorizer -mcpu=skx -S -o - -mtriple=x86_64-unknown < %s | FileCheck %s

define void @test(double* %0, double %1) {
; CHECK-LABEL: @test(
; CHECK-NEXT:    [[TMP3:%.*]] = getelementptr inbounds double, double* [[TMP0:%.*]], i32 6
; CHECK-NEXT:    br label [[TMP4:%.*]]
; CHECK:       4:
; CHECK-NEXT:    [[TMP5:%.*]] = insertelement <2 x double> <double 0.000000e+00, double poison>, double [[TMP1:%.*]], i32 1
; CHECK-NEXT:    br label [[TMP6:%.*]]
; CHECK:       6:
; CHECK-NEXT:    [[TMP7:%.*]] = load double, double* null, align 8
; CHECK-NEXT:    [[TMP8:%.*]] = insertelement <2 x double> <double 0.000000e+00, double poison>, double [[TMP7]], i32 1
; CHECK-NEXT:    [[TMP9:%.*]] = fcmp olt <2 x double> [[TMP5]], [[TMP8]]
; CHECK-NEXT:    [[TMP10:%.*]] = select <2 x i1> [[TMP9]], <2 x double> zeroinitializer, <2 x double> zeroinitializer
; CHECK-NEXT:    [[TMP11:%.*]] = fmul <2 x double> [[TMP10]], zeroinitializer
; CHECK-NEXT:    [[TMP12:%.*]] = fadd <2 x double> zeroinitializer, [[TMP11]]
; CHECK-NEXT:    [[TMP13:%.*]] = bitcast double* [[TMP3]] to <2 x double>*
; CHECK-NEXT:    store <2 x double> [[TMP12]], <2 x double>* [[TMP13]], align 8
; CHECK-NEXT:    br label [[TMP6]]
;
  %3 = getelementptr inbounds double, double* %0, i32 6
  %4 = getelementptr inbounds double, double* %0, i32 7
  br label %5

5:                                                ; preds = %2
  br label %6

6:                                                ; preds = %6, %5
  %7 = load double, double* null, align 8
  %8 = fcmp olt double 0.000000e+00, 0.000000e+00
  %9 = select i1 %8, double 0.000000e+00, double 0.000000e+00
  %10 = fcmp ogt double %7, %1
  %11 = select i1 %10, double 0.000000e+00, double 0.000000e+00
  %12 = fmul double %9, 0.000000e+00
  %13 = fmul double 0.000000e+00, %11
  %14 = fadd double 0.000000e+00, %12
  store double %14, double* %3, align 8
  %15 = fadd double 0.000000e+00, %13
  store double %15, double* %4, align 8
  br label %6
}

define { <2 x float>, <2 x float> } @test1(i32 %conv.i32.i.i.i) {
; CHECK-LABEL: @test1(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    [[CONV_I32_I_I_I1:%.*]] = fptosi float 0.000000e+00 to i32
; CHECK-NEXT:    [[TMP0:%.*]] = insertelement <4 x i32> <i32 poison, i32 0, i32 poison, i32 0>, i32 [[CONV_I32_I_I_I:%.*]], i32 0
; CHECK-NEXT:    [[TMP1:%.*]] = insertelement <4 x i32> [[TMP0]], i32 [[CONV_I32_I_I_I1]], i32 2
; CHECK-NEXT:    [[TMP2:%.*]] = icmp sgt <4 x i32> [[TMP1]], zeroinitializer
; CHECK-NEXT:    [[TMP3:%.*]] = icmp slt <4 x i32> [[TMP1]], zeroinitializer
; CHECK-NEXT:    [[TMP4:%.*]] = shufflevector <4 x i1> [[TMP2]], <4 x i1> [[TMP3]], <4 x i32> <i32 0, i32 1, i32 6, i32 3>
; CHECK-NEXT:    [[TMP5:%.*]] = select <4 x i1> [[TMP4]], <4 x float> zeroinitializer, <4 x float> zeroinitializer
; CHECK-NEXT:    [[TMP6:%.*]] = fadd <4 x float> [[TMP5]], zeroinitializer
; CHECK-NEXT:    [[TMP7:%.*]] = shufflevector <4 x float> [[TMP6]], <4 x float> poison, <2 x i32> <i32 0, i32 1>
; CHECK-NEXT:    [[TMP8:%.*]] = shufflevector <4 x float> [[TMP6]], <4 x float> poison, <2 x i32> <i32 2, i32 3>
; CHECK-NEXT:    [[DOTFCA_0_INSERT:%.*]] = insertvalue { <2 x float>, <2 x float> } zeroinitializer, <2 x float> [[TMP7]], 0
; CHECK-NEXT:    [[DOTFCA_1_INSERT:%.*]] = insertvalue { <2 x float>, <2 x float> } [[DOTFCA_0_INSERT]], <2 x float> [[TMP8]], 1
; CHECK-NEXT:    ret { <2 x float>, <2 x float> } zeroinitializer
;
entry:
  %cmp.i.i.i.i.i = icmp slt i32 0, 0
  %cond.i.i.i.i = select i1 %cmp.i.i.i.i.i, float 0.000000e+00, float 0.000000e+00
  %conv.i32.i.i.i1 = fptosi float 0.000000e+00 to i32
  %cmp.i.i34.i.i.i = icmp slt i32 %conv.i32.i.i.i1, 0
  %cond.i35.i.i.i = select i1 %cmp.i.i34.i.i.i, float 0.000000e+00, float 0.000000e+00
  %cmp.i.i38.i.i.i = icmp sgt i32 0, 0
  %cond.i39.i.i.i = select i1 %cmp.i.i38.i.i.i, float 0.000000e+00, float 0.000000e+00
  %cmp.i.i42.i.i.i = icmp sgt i32 %conv.i32.i.i.i, 0
  %cond.i43.i.i.i = select i1 %cmp.i.i42.i.i.i, float 0.000000e+00, float 0.000000e+00
  %add.i.i = fadd float 0.000000e+00, 0.000000e+00
  %add4.i.i = fadd float 0.000000e+00, 0.000000e+00
  %add.i9.i = fadd float %cond.i43.i.i.i, %add.i.i
  %retval.sroa.0.0.vec.insert4 = insertelement <2 x float> zeroinitializer, float %add.i9.i, i64 0
  %add4.i12.i = fadd float %cond.i39.i.i.i, %add4.i.i
  %retval.sroa.0.4.vec.insert7 = insertelement <2 x float> %retval.sroa.0.0.vec.insert4, float %add4.i12.i, i64 1
  %add.i15.i = fadd float %cond.i35.i.i.i, %add.i.i
  %retval.sroa.7.8.vec.insert11 = insertelement <2 x float> zeroinitializer, float %add.i15.i, i64 0
  %add4.i18.i = fadd float %cond.i.i.i.i, %add4.i.i
  %retval.sroa.7.12.vec.insert13 = insertelement <2 x float> %retval.sroa.7.8.vec.insert11, float %add4.i18.i, i64 1
  %.fca.0.insert = insertvalue { <2 x float>, <2 x float> } zeroinitializer, <2 x float> %retval.sroa.0.4.vec.insert7, 0
  %.fca.1.insert = insertvalue { <2 x float>, <2 x float> } %.fca.0.insert, <2 x float> %retval.sroa.7.12.vec.insert13, 1
  ret { <2 x float>, <2 x float> } zeroinitializer
}
