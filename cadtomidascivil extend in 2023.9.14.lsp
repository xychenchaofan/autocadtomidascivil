;小肋单元生成(xl和rib词缀均是小肋有关变量)
;小肋的节点：两个端点(由截面宽度决定)
;            选取截面后生成的xlst表(混凝土断面高度变化处对应的点)
;            和贝雷梁的弹性连接点
;在select_concrete_section命令中提取有效数据流xlst\hightlst
;xlst是截面多段线端点和中点组成的点表、hightlst是轮廓各点射线的有效高度
;xl_beamload函数用于生成小肋节点、单元和其上的混凝土/施工/模板荷载
(defun xl_beamload ()
;;;  得到多段线截面左右两个横坐标,其中xlst_pt_begin是xlst点表的第一个点，xlst_pt_end是xlst点表的最后一点  
;;;  (setq xlst (list -5 -3 -1 1 3 5 7 9 11 13 15));测试
  (setq xlst_pt_begin (nth 0 xlst)
	xlst_pt_end (nth 0 (reverse xlst))
  )
;;;  (setq xlst_pt_lst (list (list xlst_pt_begin 0 0) (list xlst_pt_end 0 0)));测试，返回((-5 0 0) (15 0 0))
;;;  把这两点数字赋给小肋的首尾，再加减模板横肋超出梁截面外边缘距离就是模型中小肋的起点和终点的y坐标(midas中的直角坐标系)
;;;  在save_dialog函数中设定了(setq relative_distance1 (get_tile "relative_distance1"))，relative_distance1在控件value中的属性是字符串
;;;  (setq relative_distance1 "15");测试
  (setq relative_distance1_atof (atof relative_distance1))
  (setq xl_pt_begin (- xlst_pt_begin relative_distance1_atof)
	xl_pt_end (+ xlst_pt_end relative_distance1_atof)
  )
;;;  (princ xlst_pt_lst)
;;;  (princ "\n")
;;;  (princ xl_pt_begin)
;;;  (princ "\n")
;;;  (princ xl_pt_end)
;;;  (princ);测试，返回((-5 0 0) (15 0 0)) -20.0 30.0
;;;  xl_pt_list—得到小肋上混凝土加载点和两个端点的y坐标(midas中的直角坐标系)
  (setq xl_pt_list1 (append (list xl_pt_begin) xlst (list xl_pt_end)))
;;;  开始建立单元,nodeid在save_dialog函数中设定了(setq nodeID (get_tile "nodeID"))，nodeid在控件value中的属性是字符串，初始化是"1"
  (setq i 0
	nodes nil
	times (length xl_pt_list1)
	elements nil
	loads nil
  )
;;;  用户输入小肋间距后使用StringRegExpS和midasdisttolst1函数生成(300 300 300 300 300 500 200 200)形式的表格，其中肋间距及数量(mm)的key是rib_spacing_distance
  (setq rib_spacing_distance_list
	 (midasdisttolst1 (get_tile "rib_spacing_distance"))
  )
;;;  得到(300 300 300 300 300 500 200 200)形式的表格后读取表格中的元素个数，根据该数量确定小肋的根数，然后开始循环
;;;  大循环是循环建立小肋单元，小循环是循环建立一根小肋的节点，第一根小肋的x坐标为0，向正轴延展
;;;  第一个模板小肋(认为x,z=0)，y坐标是xl_pt_begin/xlst/xl_pt_end三部分组成
;;;  rib_quantity是小肋数量，rib_coordinate_x是小肋横坐标，nodestring是节点流
  (setq rib_quantity (1+ (length rib_spacing_distance_list)))
  (setq rib_coordinate_x 0
	rib_codinate_x_i 0
  )
  (repeat rib_quantity;开始第一根小肋单元生成
    (setq startnodeid (atoi nodeid))
    (repeat times;开始第一根小肋节点生成
      (setq nodestring (strcat nodeid "," (itoa rib_coordinate_x) "," (rtos (nth i xl_pt_list1) 2 8) ",0"))
      (setq nodeid (itoa (+ 1 (atoi nodeid))))
      (setq nodes (append nodes (list nodestring)))
      (setq i (+ 1 i))
    )
;;; nodes返回("1,0,y1,0" "2,0,y2,0" "3,0,y3,0" ... "n,0,yn,0")，其中y0至yn是xl_pt_list1表中的原子
;;; 至此第一根小肋目标节点流已生成，接下来连接这些点成单元，小肋材料号和截面号均为1，elementid在save_dialog函数中设定了(setq elementID (get_tile "elementID"))，初始化是"1"
    (repeat (- times 1)
      (setq elementstring (strcat elementid ",BEAM,1,1," (itoa startnodeid) "," (itoa (+ 1 startnodeid)) ",0,0"))
      (setq elementid (itoa (+ 1 (atoi elementid))))
      (setq elements (append elements (list elementstring)))
      (setq startnodeid (+ 1 startnodeid))
    )
;;; 假设times=5，即一根小肋节点有5个，单元有4个，startnodeid的初始值为1
;;; elements返回("1,BEAM,1,1,1,2,0,0" "2,BEAM,1,1,2,3,0,0" "3,BEAM,1,1,3,4,0,0" "4,BEAM,1,1,4,5,0,0")
;;; rib_coordinate_x用于确定小肋横坐标，每次循环都叠加，append (list 0)是因为小肋数量的循环次数和小肋间距的循环次数差1，补0不影响最后一根小肋的x坐标并将循环次数对应
    (setq rib_coordinate_x (+ rib_coordinate_x (nth rib_codinate_x_i (append rib_spacing_distance_list (list 0)))))
    (if (<= rib_codinate_x_i (- rib_quantity 2));减2是因为表序号从0开始，repeat则是表中原子数目，两者差2
     (setq rib_codinate_x_i (1+ rib_codinate_x_i))
    )
;;; 单元生成后添加混凝土和施工模板荷载，这里考虑直线段不变截面混凝土梁
;;; 混凝土荷载由小肋间距、截面有效高度、单位长度确定
  );(repeat rib_quantity ())结束,所有小肋节点和单元生成
)


;正则表达式函数：
(defun StringRegExpS (pat str key / end keys matches x)
  (if (not *xxvbsexp)
    (setq *xxvbsexp (vlax-get-or-create-object "VBScript.RegExp"))
  )
  (vlax-put *xxvbsexp 'Pattern pat)
  (if (not key)
    (setq key "")
  )
  (setq key (strcase key))
  (setq keys '(("I" "IgnoreCase")
               ("G" "Global")
               ("M" "Multiline")
              )
  )
  (mapcar
    '(lambda (x)
       (if (wcmatch key (strcat "*" (car x) "*"))
         (vlax-put *xxvbsexp (read (cadr x)) 0)
         (vlax-put *xxvbsexp (read (cadr x)) -1)
       )
     )
    keys
  )
  (setq matches (vlax-invoke *xxvbsexp 'Execute str))
  (vlax-for x matches (setq end (cons (vla-get-value x) end)))
  (reverse end)
)


;该函数midasdisttolst1有修正：
;(setq temp2 (append temp2 (list "1")))
;(repeat temp4 (setq temp5 (append temp5 (list temp3))))
(defun midasdisttolst1 (txt / temp1 temp2 temp3 temp4 temp5)
  (setq temp1 (StringRegExpS "[^,，+ ]+" txt ""))
  (setq temp5 nil)
  (foreach x temp1
    (setq temp2 (StringRegExpS "[^×@*]+" x ""))
    (if	(= (length temp2) 1)
      (setq temp2 (append temp2 (list "1")))
    )
    (setq temp3 (atoi (car temp2)))
    (setq temp4 (atoi (last temp2)))
    (repeat temp4 (setq temp5 (append temp5 (list temp3))))
  )
  temp5
)


;对正则表达式进行测试
;;;(defun c:test1 ()
;;;  (setq txt "300*5+500+200*2")
;;;  (setq temp1 (StringRegExpS "[^,，+ ]+" txt ""))
;;;)
;返回("300*5" "500" "200*2")
;;;(defun c:test2 ()
;;;  (setq list1 (midasdisttolst1 "300*5+500+200*2"))
;;;  (setq list2 (midasdisttolst1 "450*3+900*2+450*3"))
;;;  (setq list3 (midasdisttolst1 "1500+3000*7+1500"))
;;;  (setq list4 (midasdisttolst1 "3000+3000"))
;;;  (setq list5 (midasdisttolst1 "5000"))
;;;  (setq list6 (midasdisttolst1 "12000+3000+12000"))
;;;)
;返回结果如下
;;;(300 300 300 300 300 500 200 200)
;;;(450 450 450 900 900 450 450 450)
;;;(1500 3000 3000 3000 3000 3000 3000 3000 1500)
;;;(3000 3000)
;;;(5000)
;;;(12000 3000 12000)
;测试结束


;;;建立小肋截面库，rib—小肋，sections—所有截面，section1—单截面，section2—双截面
(defun rib_sections()
    (setq rib_section1
	   '("DBUSER,I10,CC,0,0,0,0,0,0,YES,NO,H,2,100,68,4.5,7.6,0,0,6.5,0,0,0"
	     "DBUSER,I12,CC,0,0,0,0,0,0,YES,NO,H,2,120,74,5,8.4,0,0,7,0,0,0"
	     "DBUSER,I12.6,CC,0,0,0,0,0,0,YES,NO,H,2,126,74,5,8.4,0,0,7,0,0,0"
	     "DBUSER,I14.6,CC,0,0,0,0,0,0,YES,NO,H,2,140,80,5.5,9.1,0,0,7.5,0,0,0"
	     "DBUSER,I16,CC,0,0,0,0,0,0,YES,NO,H,2,160,88,6,9.9,0,0,8,0,0,0"
	     "DBUSER,I18,CC,0,0,0,0,0,0,YES,NO,H,2,180,94,6.5,10.7,0,0,8.5,0,0,0"
	     "DBUSER,I20a,CC,0,0,0,0,0,0,YES,NO,H,2,200,100,7,11.4,0,0,9,0,0,0"
	     "DBUSER,I20b,CC,0,0,0,0,0,0,YES,NO,H,2,200,102,9,11.4,0,0,9,0,0,0"
	     "DBUSER,I22a,CC,0,0,0,0,0,0,YES,NO,H,2,220,110,7.5,12.3,0,0,9.5,0,0,0"
	     "DBUSER,I22b,CC,0,0,0,0,0,0,YES,NO,H,2,220,112,9.5,12.3,0,0,9.5,0,0,0"
	     "DBUSER,I24a,CC,0,0,0,0,0,0,YES,NO,H,2,240,116,8,13,0,0,10,0,0,0"
	     "DBUSER,I24b,CC,0,0,0,0,0,0,YES,NO,H,2,240,116,8,13,0,0,10,0,0,0"
	     "DBUSER,I25a,CC,0,0,0,0,0,0,YES,NO,H,2,250,116,8,13,0,0,10,0,0,0"
	     "DBUSER,I25b,CC,0,0,0,0,0,0,YES,NO,H,2,250,118,10,13,0,0,10,0,0,0"
	     "DBUSER,I27a,CC,0,0,0,0,0,0,YES,NO,H,2,270,122,8.5,13.7,0,0,10.5,0,0,0"
	     "DBUSER,I27b,CC,0,0,0,0,0,0,YES,NO,H,2,270,124,10.5,13.7,0,0,10.5,0,0,0"
	     "DBUSER,I28a,CC,0,0,0,0,0,0,YES,NO,H,2,280,122,8.5,13.7,0,0,10.5,0,0,0"
	     "DBUSER,I28b,CC,0,0,0,0,0,0,YES,NO,H,2,280,124,10.5,13.7,0,0,10.5,0,0,0"
	     "DBUSER,I30a,CC,0,0,0,0,0,0,YES,NO,H,2,300,126,9,14.4,0,0,11,0,0,0"
	     "DBUSER,C5,CC,0,0,0,0,0,0,YES,NO,C,2,50,37,4.5,7,37,7,7,3.5,0,0"
	     "DBUSER,C6.3,CC,0,0,0,0,0,0,YES,NO,C,2,63,40,4.8,7.5,40,7.5,7.5,3.8,0,0"
  	     "DBUSER,C6.5,CC,0,0,0,0,0,0,YES,NO,C,2,65,40,4.3,7.5,40,7.5,7.5,3.8,0,0"
 	     "DBUSER,C8,CC,0,0,0,0,0,0,YES,NO,C,2,80,43,5,8,43,8,8,4,0,0"
	     "DBUSER,C10,CC,0,0,0,0,0,0,YES,NO,C,2,100,48,5.3,8.5,48,8.5,8.5,4.2,0,0"
	     "DBUSER,C12,CC,0,0,0,0,0,0,YES,NO,C,2,120,53,5.5,9,53,9,9,9,0,0"
 	     "DBUSER,C12.6,CC,0,0,0,0,0,0,YES,NO,C,2,126,53,5.5,9,53,9,9,4.5,0,0"
	     "DBUSER,C14a,CC,0,0,0,0,0,0,YES,NO,C,2,140,58,6,9.5,58,9.5,9.5,4.8,0,0"
	     "DBUSER,C14b,CC,0,0,0,0,0,0,YES,NO,C,2,140,60,8,9.5,60,9.5,9.5,5,0,0"
	     "DBUSER,C16a,CC,0,0,0,0,0,0,YES,NO,C,2,160,63,6.5,10,63,10,10,5,0,0"
	     "DBUSER,C16b,CC,0,0,0,0,0,0,YES,NO,C,2,160,65,8.5,10,65,10,10,5,0,0"
	     "DBUSER,C18a,CC,0,0,0,0,0,0,YES,NO,C,2,180,68,7,10.5,68,10.5,10.5,5.2,0,0"
	     "DBUSER,C18b,CC,0,0,0,0,0,0,YES,NO,C,2,180,70,9,10.5,70,10.5,10.5,5.2,0,0"
	     "DBUSER,C20a,CC,0,0,0,0,0,0,YES,NO,C,2,200,73,7,11,73,11,11,5.5,0,0"
	     "DBUSER,C20b,CC,0,0,0,0,0,0,YES,NO,C,2,200,75,9,11,75,11,11,5.5,0,0"
	     "DBUSER,C22a,CC,0,0,0,0,0,0,YES,NO,C,2,220,77,7,11.5,77,11.5,11.5,5.8,0,0"
	     "DBUSER,C22b,CC,0,0,0,0,0,0,YES,NO,C,2,220,79,9,11.5,79,11.5,11.5,5.8,0,0"
	     "DBUSER,C24a,CC,0,0,0,0,0,0,YES,NO,C,2,240,78,7,12,78,12,12,6,0,0"
	     "DBUSER,C24b,CC,0,0,0,0,0,0,YES,NO,C,2,240,80,9,12,80,12,12,6,0,0"
	     "DBUSER,C24c,CC,0,0,0,0,0,0,YES,NO,C,2,240,82,11,12,82,12,12,6,0,0"
	     "DBUSER,C25a,CC,0,0,0,0,0,0,YES,NO,C,2,250,78,7,12,78,12,12,6,0,0"
 	     "DBUSER,C25b,CC,0,0,0,0,0,0,YES,NO,C,2,250,80,9,12,80,12,12,6,0,0"
 	     "DBUSER,C25c,CC,0,0,0,0,0,0,YES,NO,C,2,250,82,11,12,82,12,12,6,0,0"
	     "DBUSER,C27a,CC,0,0,0,0,0,0,YES,NO,C,2,270,82,7.5,12.5,82,12.5,12.5,6.2,0,0"
	     "DBUSER,C27b,CC,0,0,0,0,0,0,YES,NO,C,2,270,84,9.5,12.5,84,12.5,12.5,6.2,0,0"
	     "DBUSER,C27c,CC,0,0,0,0,0,0,YES,NO,C,2,270,86,11.5,12.5,86,12.5,12.5,6.2,0,0"
	     "DBUSER,C28a,CC,0,0,0,0,0,0,YES,NO,C,2,280,82,7.5,12.5,82,12.5,12.5,6.2,0,0"
	     "DBUSER,C28b,CC,0,0,0,0,0,0,YES,NO,C,2,280,84,9.5,12.5,84,12.5,12.5,6.2,0,0"
	     "DBUSER,C28c,CC,0,0,0,0,0,0,YES,NO,C,2,280,86,11.5,12.5,86,12.5,12.5,6.2,0,0"
	     "DBUSER,C30a,CC,0,0,0,0,0,0,YES,NO,C,2,300,85,7.5,13.5,85,13.5,13.5,6.8,0,0"
	     "DBUSER,C30b,CC,0,0,0,0,0,0,YES,NO,C,2,300,87,9.5,13.5,87,13.5,13.5,6.8,0,0"
	     "DBUSER,C30c,CC,0,0,0,0,0,0,YES,NO,C,2,300,89,11.5,13.5,89,13.5,13.5,6.8,0,0"
	      )
	  rib_section2
	   '("DBUSER,2C5,CC,0,0,0,0,0,0,YES,NO,2C,2,50,37,4.5,7,0,0,0,0,0,0"
	     "DBUSER,2C6.3,CC,0,0,0,0,0,0,YES,NO,2C,2,63,40,4.8,7.5,0,0,0,0,0,0"
	     "DBUSER,2C8,CC,0,0,0,0,0,0,YES,NO,2C,2,80,43,5,8,0,0,0,0,0,0"
	     "DBUSER,2C10,CC,0,0,0,0,0,0,YES,NO,2C,2,100,48,5.3,8.5,0,0,0,0,0,0"
 	     "DBUSER,2C12.6,CC,0,0,0,0,0,0,YES,NO,2C,2,126,53,5.5,9,0,0,0,0,0,0"
	     "DBUSER,2C14a,CC,0,0,0,0,0,0,YES,NO,2C,2,140,58,6,9.5,0,0,0,0,0,0"
	     "DBUSER,2C14b,CC,0,0,0,0,0,0,YES,NO,2C,2,140,60,8,9.5,0,0,0,0,0,0"
	     "DBUSER,2C16a,CC,0,0,0,0,0,0,YES,NO,2C,2,160,63,6.5,10,0,0,0,0,0,0"
	     "DBUSER,2C16b,CC,0,0,0,0,0,0,YES,NO,2C,2,160,65,8.5,10,0,0,0,0,0,0"
	     "DBUSER,2C18a,CC,0,0,0,0,0,0,YES,NO,2C,2,180,68,7,10.5,0,0,0,0,0,0"
	     "DBUSER,2C18b,CC,0,0,0,0,0,0,YES,NO,2C,2,180,70,9,10.5,0,0,0,0,0,0"
	     "DBUSER,2C20a,CC,0,0,0,0,0,0,YES,NO,2C,2,200,73,7,11,0,0,0,0,0,0"
	     "DBUSER,2C20b,CC,0,0,0,0,0,0,YES,NO,2C,2,200,75,9,11,0,0,0,0,0,0"
	     "DBUSER,2C22a,CC,0,0,0,0,0,0,YES,NO,2C,2,220,77,7,11.5,0,0,0,0,0,0"
	     "DBUSER,2C22b,CC,0,0,0,0,0,0,YES,NO,2C,2,220,79,9,11.5,0,0,0,0,0,0"
	     "DBUSER,2C25a,CC,0,0,0,0,0,0,YES,NO,2C,2,250,78,7,12,0,0,0,0,0,0"
	     "DBUSER,2C25b,CC,0,0,0,0,0,0,YES,NO,2C,2,250,80,9,12,0,0,0,0,0,0"
	     "DBUSER,2C25c,CC,0,0,0,0,0,0,YES,NO,2C,2,250,82,11,12,0,0,0,0,0,0"
	     "DBUSER,2C28a,CC,0,0,0,0,0,0,YES,NO,2C,2,280,82,7.5,12.5,0,0,0,0,0,0"
	     "DBUSER,2C28b,CC,0,0,0,0,0,0,YES,NO,2C,2,280,84,9.5,12.5,0,0,0,0,0,0"
	     "DBUSER,2C28c,CC,0,0,0,0,0,0,YES,NO,2C,2,280,86,11.5,12.5,0,0,0,0,0,0"
 	     "DBUSER,2C32a,CC,0,0,0,0,0,0,YES,NO,2C,2,320,88,8,14,0,0,0,0,0,0"
	     "DBUSER,2C32b,CC,0,0,0,0,0,0,YES,NO,2C,2,320,90,10,14,0,0,0,0,0,0"
	     "DBUSER,2C32c,CC,0,0,0,0,0,0,YES,NO,2C,2,320,92,12,14,0,0,0,0,0,0"
	     "DBUSER,2C36a,CC,0,0,0,0,0,0,YES,NO,2C,2,360,96,9,16,0,0,0,0,0,0"
	     "DBUSER,2C36b,CC,0,0,0,0,0,0,YES,NO,2C,2,360,98,11,16,0,0,0,0,0,0"
	     "DBUSER,2C36c,CC,0,0,0,0,0,0,YES,NO,2C,2,360,100,13,16,0,0,0,0,0,0"
	     "DBUSER,2C40a,CC,0,0,0,0,0,0,YES,NO,2C,2,400,100,10.5,18,0,0,0,0,0,0"
	     "DBUSER,2C40b,CC,0,0,0,0,0,0,YES,NO,2C,2,400,102,12.5,18,0,0,0,0,0,0"
	     "DBUSER,2C40c,CC,0,0,0,0,0,0,YES,NO,2C,2,400,104,14.5,18,0,0,0,0,0,0"
	     )
	  )
)


;;;建立分配梁截面库，distributed—分配梁，sections—所有截面，section1—单截面，section2—双截面
(defun distributed_sections()
  (setq distributed_section1
	   '("DBUSER,I45a,CC,0,0,0,0,0,0,YES,NO,H,2,450,150,11.5,18,0,0,13.5,0,0,0"
	     "DBUSER,HM588X300,CC,0,0,0,0,0,0,YES,NO,H,2,588,300,12,20,0,0,13,0,0,0"
	     "DBUSER,HN700X300,CC,0,0,0,0,0,0,YES,NO,H,2,700,300,13,24,0,0,18,0,0,0"
	     "DBUSER,HN900X300,CC,0,0,0,0,0,0,YES,NO,H,2,900,300,16,28,0,0,18,0,0,0"
	    )
	distributed_section2
	   '("DBUSER,2I45a,CC,0,0,0,0,0,0,YES,NO,B,2,450,300,11.5,18,150,18,0,0,0,0"
	     "DBUSER,2HM588X300,CC,0,0,0,0,0,0,YES,NO,B,2,588,600,12,20,300,20,0,0,0,0"
 	     "DBUSER,2HN700X300,CC,0,0,0,0,0,0,YES,NO,B,2,700,600,13,24,300,24,0,0,0,0"
	     "DBUSER,2HN750X300,CC,0,0,0,0,0,0,YES,NO,B,2,750,600,13,24,300,24,0,0,0,0"
 	     "DBUSER,2HN800X300,CC,0,0,0,0,0,0,YES,NO,B,2,800,600,14,26,300,26,0,0,0,0"
	     "DBUSER,2HN850X300,CC,0,0,0,0,0,0,YES,NO,B,2,850,600,16,27,300,27,0,0,0,0"
	     "DBUSER,2HN890X299,CC,0,0,0,0,0,0,YES,NO,B,2,890,598,15,23,299,23,0,0,0,0"
	     "DBUSER,2HN900X300,CC,0,0,0,0,0,0,YES,NO,B,2,900,600,16,28,300,28,0,0,0,0"
	     "DBUSER,2HN970X297,CC,0,0,0,0,0,0,YES,NO,B,2,970,594,16,21,297,21,0,0,0,0"
	     "DBUSER,2HN980X298,CC,0,0,0,0,0,0,YES,NO,B,2,980,596,17,26,298,26,0,0,0,0"
	     "DBUSER,2HN990X298,CC,0,0,0,0,0,0,YES,NO,B,2,990,596,17,31,298,31,0,0,0,0"
	     "DBUSER,2HN1000X300,CC,0,0,0,0,0,0,YES,NO,B,2,1000,600,19,36,300,36,0,0,0,0"
	    )
  )
)


;;;建立钢管桩截面库(改用户自己输入，不建立库)，steelpile—钢管桩，section—截面
;;;steelpile_section在下面support_section_list函数中已经被用掉了
;;;(defun steelpile_sections()
;;;  (setq steelpile_section
;;;	 '("DBUSER,钢管桩φ630x8,CC,0,0,0,0,0,0,YES,NO,P,2,630,8,0,0,0,0,0,0,0,0"
;;;	   "DBUSER,钢管桩φ820x10,CC,0,0,0,0,0,0,YES,NO,P,2,820,10,0,0,0,0,0,0,0,0"
;;;	  )
;;;  )
;;;)


;;;建立钢管桩联结系截面库(先不用)，bracing—联结系，sections—所有截面，section1—单截面，section2—双截面
(defun bracing_sections()
  (setq bracing_section1
	 '("DBUSER,2C20a,CC,0,0,0,0,0,0,YES,NO,2C,1,GB-YB05,2C 20a"
	   "DBUSER,2C14a,CC,0,0,0,0,0,0,YES,NO,2C,1,GB-YB05,2C 14a"
	  )
	bracing_section2
	 '("DBUSER,C 20a,CC,0,0,0,0,0,0,YES,NO,C,1,GB-YB05,C 20a"
	   "DBUSER,C 14a,CC,0,0,0,0,0,0,YES,NO,C,1,GB-YB05,C 14a"
	  )
  )
)


;;;钢管贝雷梁支架截面库(根据用户在表中所选数据生成一张截面库表，表中每一项均是字符串)
;;;1-小肋截面；2-贝雷梁弦杆；3-贝雷梁竖杆；4-贝雷梁斜杆；5-支撑架弦杆竖杆；6-支撑架斜杆；7-分配梁截面；8-钢管桩截面；9-钢管桩联结系2C20a弦杆；10-钢管桩联结系C20a竖杆斜杆
;;;变量support_section_chosen存储用户选择或固定设置的截面信息，support—支架，section—截面，chosen—被选择
;;;设置10个sec变量一一对应每个截面序号方便后续变量直接取用和修改，sec——section缩写
(defun support_section_list()
  (setq sec1 1 sec2 2 sec3 3 sec4 4 sec5 5 sec6 6 sec7 7 sec8 8 sec9 9 sec10 10)
;;;写入小肋截面,当控件rib_double_section的值为0时，说明点选了单截面按钮
;;;"rib_double_section"—控件小肋双截面按钮的key，rib_section_chosen—用户选择的小肋截面是(string)
;;;rib_section_name1—小肋截面下拉列表中小肋单截面的表项序号(string)，rib_section1—小肋单截面库(list)
;;;rib_section_name2—小肋截面下拉列表中小肋双截面的表项序号(string)，rib_section2—小肋双截面库(list)
  (if (= (get_tile "rib_double_section") 0)
      (progn
	(setq rib_section_chosen (strcat (itoa sec1) ", " (nth (atoi rib_section_name1) rib_section1)))
      )
      (progn
	(setq rib_section_chosen (strcat (itoa sec1) ", " (nth (atoi rib_section_name2) rib_section2)))
      )
  )
;;;写入贝雷梁和支撑架截面
;;;Bailey—贝雷梁，section1/2/3—各种对应截面，L —L型钢(支撑架)
  (setq Bailey_section1 (strcat (itoa sec2) ", " "DBUSER, 贝雷梁弦杆, CC, 0, 0, 0, 0, 0, 0, YES, NO, 2C , 2, 100, 48, 5.3, 8.5, 80, 0, 0, 0, 0, 0"))
  (setq Bailey_section2 (strcat (itoa sec3) ", " "DBUSER, 贝雷梁竖杆, CC, 0, 0, 0, 0, 0, 0, YES, NO, H , 2, 80, 50, 4.5, 6.5, 0, 0, 0, 0, 0, 0"))
  (setq Bailey_section3 (strcat (itoa sec4) ", " "DBUSER, 贝雷梁斜杆, CC, 0, 0, 0, 0, 0, 0, YES, NO, H , 2, 80, 50, 4.5, 6.5, 0, 0, 0, 0, 0, 0"))
  (setq L_section1 (strcat (itoa sec5) ", " "DBUSER, L 75x50x6, CC, 0, 0, 0, 0, 0, 0, YES, NO, L , 2, 75, 50, 6, 6, 0, 0, 0, 0, 0, 0"))
  (setq L-section2 (strcat (itoa sec6) ", " "DBUSER, L 40x5, CC, 0, 0, 0, 0, 0, 0, YES, NO, L , 1, GB-YB05, L 40x5"))
;;;写入分配梁截面，当控件distributed_single_section的值为0时，说明用户选择了双截面
;;;"distributed_single_section"—控件分配梁单截面按钮的key，distributed_section_chosen—用户选择的分配梁截面是(string)
;;;distributed_single_section_number—分配梁截面下拉列表中单截面的表项序号(string)，distributed_section1—分配梁单截面库(list)
;;;distributed_double_section_number—分配梁截面下拉列表中双截面的表项序号(string)，distributed_section2—分配梁双截面库(list)
  (if (= (get_tile "distributed_single_section") 0)
      (progn
	(setq distributed_section_chosen (strcat (itoa sec7) ", " (nth (atoi distributed_double_section_number) distributed_section2)))
      )
      (progn
	(setq distributed_section_chosen (strcat (itoa sec7) ", " (nth (atoi distributed_single_section_number) distributed_section1)))
      )
  )
;;;写入钢管桩截面，该截面由用户输入后获得
;;;钢管桩直径steelpile_diameter由(get_tile "steelpile_diameter")获得,"steelpile_diameter"是key，返回(string)
;;;钢管桩壁厚steelpile_thickness由(get_tile "steelpile_thickness")获得，"steelpile_thickness"是key，返回(string)
;;;(setq sec8 8 steelpile_diameter "820" steelpile_thickness "10");测试
  (setq steelpile_section (strcat (itoa sec8) ", " "DBUSER, " "钢管桩" steelpile_diameter "x" steelpile_thickness ", CC, 0, 0, 0, 0, 0, 0, YES, NO, P , 2, " steelpile_diameter ", " steelpile_thickness ", 0, 0, 0, 0, 0, 0, 0, 0"))
;;;写入钢管桩联结系截面(后续也可以让用户在一定范围内自己选择)
;;;2C20a—双C20a，C_20a—单槽20a
  (setq 2C20a_section (strcat (itoa sec9) ", " "DBUSER, 2C20a, CC, 0, 0, 0, 0, 0, 0, YES, NO, 2C , 1, GB-YB05, 2C 20a")
	C_20a_section (strcat (itoa sec10) ", " "DBUSER, C 20a, CC, 0, 0, 0, 0, 0, 0, YES, NO, C  , 1, GB-YB05, C 20a")
  )
;;;得到所有截面信息后开始建立总表
  (setq support_section_chosen
	 (list
	   rib_section_chosen
	   Bailey_section1
	   Bailey_section2
	   Bailey_section3
	   L_section1
	   L-section2
	   distributed_section_chosen
	   steelpile_section
	   2C20a_section
	   C_20a_section
	 )
  )
);返回("1, ..." "2, ..." ... "10, ..." ...)，表中每一个截面信息对应的字符串是表中的一个原子


;;;(defun c:a1 ();测试
;;;  (setq a (getint "\n输入一个整数："))
;;;  (setq list1 (list
;;;	        (if (< a 1)
;;;		  (progn (setq b1 "cc"))
;;;		  (progn (setq b1 "aa"))
;;;		)
;;;	      ))
;;;  )
;;;(defun c:test1()
;;;  (setq startnodeid 1)
;;;  (setq elementid "1")
;;;  (setq elements nil)
;;;  (repeat 4
;;;      (setq elementstring (strcat elementid ",BEAM,1,1," (itoa startnodeid) "," (itoa (+ 1 startnodeid)) ",0,0"))
;;;      (setq elementid (itoa (+ 1 (atoi elementid))))
;;;      (setq startnodeid (+ 1 startnodeid))
;;;      (setq elements (append elements (list elementstring)))
;;;    )
;;;  )
;;;(defun c:test2()
;;;  (setq rib_codinate_x_i 0)
;;;  (setq rib_coordinate_x 0)
;;;  (setq rib_quantity 9)
;;;  (repeat rib_quantity
;;;    (setq rib_coordinate_x (+ rib_coordinate_x (nth rib_codinate_x_i (list 300 300 300 300 300 500 200 200 0))))
;;;    (if (<= rib_codinate_x_i (- rib_quantity 2))
;;;     (setq rib_codinate_x_i (1+ rib_codinate_x_i))
;;;    )
;;;  )
;;;  (princ rib_coordinate_x)
;;;  )


;;;主程序扩展
;;;这段程序用lisp来模拟dcl文件，此时需要一个临时文件区来写入这个文件
(defun c:CTM (/ dclname tempname filen stream dcl_re)
  (setvar "cmdecho" 0)
  (setq sdt nil)
  (while (and (/= sdt 0) (/= sdt 1))
;;;sdt=0卸载对话框，sdt=1生成mct文件，当同时成立时，说明用户没有需要对话框来设置参数化，此时对话框文件开始加载
    (setq dclname
	   (cond
	     ((setq tempname (vl-filename-mktemp "cadtomidascivil.dcl");返回磁盘目录下的该文件路径并转换为字符串
		    filen    (open tempname "w");返回tempname的文件描述符
	      )
	      (foreach stream;stream作为形参通过foreach函数将list中每一个字符串依次写入
		    (list "\n"
			  "cadtomidascivil:dialog {                                                                                                    \n";生成对话框，对话框的名字是cadtomidascivil
			  " label=\"钢管贝雷梁支架参数化建模V0.1-荷载\";                                                                               \n";对话框标签"是钢管贝雷梁支架参数化建模V0.1-荷载"
;;;;;;			  添加起始编号
;;;			  " :boxed_column { label=\"起始编号：\";                                                                                      \n";加框列1-标签"起始编号"
;;;			  " :row {                                                                                                                     \n";加框列1的第1行
;;;                          " :edit_box  { label=\"节点号：\"; key = \"nodeID\"; value=\"1\";}                                                           \n";编辑框-标签"节点号"-关键字nodeID-值1
;;;			  " :edit_box  { label=\"单元号：\"; key = \"elementID\"; value=\"1\";}                                                        \n";编辑框-标签"单元号"-关键字elementID-值1
;;;			  " :edit_box  { label=\"截面号：\"; key = \"sectionID\"; value=\"1\";}                                                        \n";编辑框-标签"截面号"-关键字sectionID-值1
;;;			  "      }                                                                                                                     \n";加框列1第1行结束
;;;			  " :row {                                                                                                                     \n";加框列1的第2行
;;;			  " :edit_box  { label=\"材料号：\"; key = \"materialID\"; value=\"1\";}                                                       \n";编辑框-标签"材料号"-关键字materialID-值1
;;;			  " :button { key=\"resetid\";label=\"重置为1\"; }                                                                             \n";按钮-标签"重置为1"-关键字resetid
;;;			  " :button { key=\"automaticreadid\";label=\"从mct中自动识别\"; }                                                             \n";按钮-标签"从mct中自动识别"-关键字automaticreadid
;;;			  "      }                                                                                                                     \n";加框列1第2行结束
;;;			  "               }                                                                                                            \n";加框列1结束
;;;			  一、添加小肋的参数设置
			  " :boxed_column { label=\"小肋模板参数设置：\";                                                                              \n"
;;;			  1、添加模板横肋
			  " :boxed_column { label=\"模板横肋：\";                                                                                      \n"
		          " :row {                                                                                                                     \n"
			  " :popup_list { label=\"截面类型\"; key= \"rib_section_list\";width=15;fixed_width = true;}                                  \n"
		          " :radio_button { label=\"单截面\"; key=\"rib_single_section\";}                                                             \n"
		          " :radio_button { label=\"双拼截面\"; key=\"rib_double_section\";}                                                           \n"
		          "      }                                                                                                                     \n"
			  " :row {                                                                                                                     \n"
			  " :edit_box  { label=\"横肋间距及数量(mm)：\"; key = \"rib_spacing_distance\";value=\"例300*5+500+200*2\";}                  \n"
			  "      }                                                                                                                     \n"
			  "               }                                                                                                            \n"
;;;			  2、添加混凝土和施工模板荷载
			  " :boxed_row { label=\"混凝土荷载：\";                                                                                       \n"
			  " :edit_box  { label=\"混凝土容重(kN/m3)：\"; key = \"concretedensity\"; value=\"26.5\"; }                                   \n"
			  " :button { label=\"选择截面\"; key=\"selectsection\"; }                                                                     \n"
			  "            }                                                                                                               \n"
			  " :boxed_row { label=\"施工、模板荷载：\";                                                                                   \n"
			  " :edit_box  { label=\"施工荷载(kN/m2)：\"; key = \"construction_load\"; value=\"2\"; }                                      \n"
			  " :edit_box  { label=\"模板荷载(kN/m2)：\"; key = \"template_load\"; value=\"2.5\"; }                                        \n"
			  "            }                                                                                                               \n"
;;;			  1和2结束
			  "               }                                                                                                            \n"
;;;			  二、添加贝雷梁加框列
			  " :boxed_column { label=\"贝雷梁参数设置：\";                                                                                \n"
			  " :edit_box  { label=\"贝雷梁间距及数量(mm)：\"; key = \"BaileyBeam_distance\"; value=\"例450*3+900*2+450*3\";}              \n"
			  " :edit_box  { label=\"贝雷梁纵桥向布置(mm)：\"; key = \"BaileyBeam_span\"; value=\"例1500+3000*7+1500\";}                   \n"
			  "               }                                                                                                            \n"
;;;			  三、添加分配梁加框行
			  " :boxed_row { label=\"分配梁参数设置：\";                                                                                   \n"
			  " :popup_list { label=\"分配梁类型\"; key= \"distributed_section_list\";width=30;fixed_width = true;}                        \n"
			  " :radio_row {                                                                                                               \n"
			  "  key = \"distributed_section_button\";                                                                                     \n"
			  " :radio_button { label=\"单截面\"; key=\"distributed_single_section\"; mnemonic= \"0\"; value=0;}                           \n"
			  " :radio_button { label=\"双截面\"; key=\"distributed_double_section\"; mnemonic= \"1\"; value=1;}                           \n"
			  "            }                                                                                                               \n"
			  "            }                                                                                                               \n"
;;;			 四、 添加钢管桩加框列
			  " :boxed_column { label=\"钢管桩参数设置：\";                                                                                \n"
			  " :boxed_row { label=\"钢管桩规格：\";                                                                                       \n"
			  " :edit_box  { label=\"钢管桩直径(mm)：\"; key = \"steelpile_diameter\"; value=\"820\"; }                                    \n"
			  " :edit_box  { label=\"钢管桩壁厚(mm)：\"; key = \"steelpile_thickness\"; value=\"10\"; }                                    \n"
			  " :edit_box  { label=\"桩长(mm)：\"; key = \"steelpile_length\"; value=\"例20000\"; }                                        \n"
			  "            }                                                                                                               \n"		  
			  " :row {                                                                                                                     \n"
			  " :boxed_column { label=\"横桥向参数设置：\";                                                                                \n"
			  " :row {                                                                                                                     \n"
			  " :edit_box  { label=\"间距(mm)：\"; key = \"steelpile_lateral_distance\"; width=30;value=\"例3000+3000\"; }                 \n"
			  " :edit_box  { label=\"管桩中心偏移（默认对称居中,mm）：\"; key = \"skew_distance\"; value=\"0\"; }                          \n"
			  "      }                                                                                                                     \n"
			  "               }                                                                                                            \n"
			  "      }                                                                                                                     \n"
			  " :row {                                                                                                                     \n"
			  " :boxed_row { label=\"纵桥向参数设置：\";                                                                                   \n"
			  " :column {                                                                                                                  \n"
			  " :edit_box  { label=\"桩顶间距(mm)：\"; key = \"steelpile_lengthways_distance1\"; value=\"例12000+3000+12000\"; }           \n"
			  " :edit_box  { label=\"桩底间距(mm)：\"; key = \"steelpile_lengthways_distance2\"; value=\"例12000+3000+12000\"; }           \n"
			  "         }                                                                                                                  \n"
			  " :column {                                                                                                                  \n"
			  " :edit_box  { label=\"贝雷左端悬出(mm)：\"; key = \"leftout_distance\"; value=\"例0\"; }                                    \n"
			  " :edit_box  { label=\"贝雷右端悬出(mm)：\"; key = \"rightout_distance\"; value=\"例0\"; }                                   \n"
			  "         }                                                                                                                  \n"
			  "               }                                                                                                            \n"
			  "      }                                                                                                                     \n"
			  "               }                                                                                                            \n"
;;;			  五、添加结构相对位置关系
			  " :boxed_column { label=\"各结构相对位置关系：\";                                                                            \n"
			  " :edit_box  { label=\"模板横肋超出梁截面外边缘(施工荷载加载范围，mm)：\"; key = \"relative_distance1\"; value=\"1500\"; }   \n"
			  " :row {                                                                                                                     \n"
			  " :edit_box  { label=\"分配梁相对贝雷悬出(mm)：\"; key = \"overhang_distance\"; value=\"例1500\"; }                          \n"
			  " :edit_box  { label=\"管桩相对分配梁缩进(mm)：\"; key = \"retracted_distance\"; value=\"例1500\"; }                         \n"
			  "      }                                                                                                                     \n"
			  "               }                                                                                                            \n"
;;;			  六、添加底部按钮
			  " :row {                                                                                                                     \n";行1
			  "          :button { key=\"ok\";label=\"生成\"; }                                                                            \n";生成按钮
			  "          :button { key=\"save\";label=\"暂存\"; }                                                                          \n";暂存按钮
			  "          :button { key=\"exit\";label=\"退出\"; is_cancel=true; }                                                          \n";退出按钮
			  "      }                                                                                                                     \n";行1结束
			  "}                                                                                                                           \n";对话框结束
	        )
		(princ stream filen);(foreach stream (list 字符串) (princ stream filename))
	      );foreach stream ...结束
	      (close filen);关闭对应的文件
	      tempname
	     );cond结束，此时list中的字符串皆写入到dclname中，实现lisp模拟dcl对话框文件
	   );setq dclname结束
    );while (and (/= sdt 0) (/= sdt 1)结束
    (setq dcl_re (load_dialog dclname));加载dclname对话框(lisp模拟dcl文件)并赋值给dcl_re
    (if	(not (new_dialog "cadtomidascivil" dcl_re));如果没有打开dcl_re中名字为cadtomidascivil的对话框文件，退出程序
      (exit)
    )
;;;初始化文本框
;;;如果ID不存在，则以1为建模的初始节点，如果某ID已经存在，以其为起始ID
;;;目前记录的ID有nodeID、elementID、sectionID、materialID、梁单元荷载beamloadID，厚度thicknessID
;;;还缺结构组groupID、边界组BNDR-groupID、荷载组LOAD-groupID、节点荷载conloadID、一般支撑constraintID ,弹性连接elasticlinkID
;;;    ;;;设定【节点】编号起始值
;;;    (if (= nodeID nil) (set_tile "nodeID" "1") (set_tile "nodeID" nodeID))
;;;    ;;;设定【单元】编号起始值
;;;    (if (= elementID nil) (set_tile "elementID" "1") (set_tile "elementID" elementID))
;;;    ;;;设定【截面】编号起始值
;;;    (if (= sectionID nil) (set_tile "sectionID" "1") (set_tile "sectionID" sectionID))
;;;    ;;;设定【材料】编号起始值
;;;    (if (= materialID nil) (set_tile "materialID" "1") (set_tile "materialID" materialID))
    ;;;设定【横肋间距及数量】起始值
    (if (= rib_spacing_distance nil) (set_tile "rib_spacing_distance" "例300*5+500+200*2") (set_tile "rib_spacing_distance" rib_spacing_distance))
    ;;;设定【混凝土重度】初始值
    (if (= concretedensity nil) (set_tile "concretedensity" "26.5") (set_tile "concretedensity" concretedensity))
    ;;;设定【施工荷载】初始值
    (if (= construction_load nil) (set_tile "construction_load" "2") (set_tile "construction_load" construction_load))
    ;;;设定【模板荷载】初始值
    (if (= template_load nil) (set_tile "template_load" "2.5") (set_tile "template_load" template_load))
    ;;;设定【模板横肋超出梁截面外边缘】初始值
    (if (= relative_distance1 nil) (set_tile "relative_distance1" "1500") (set_tile "relative_distance1" relative_distance1))
    ;;;设定【分配梁相对贝雷悬出】初始值
    (if (= overhang_distance nil) (set_tile "overhang_distance" "例1500") (set_tile "overhang_distance" overhang_distance))
    ;;;设定【管桩相对分配梁缩进】初始值
    (if (= retracted_distance nil) (set_tile "retracted_distance" "例1500") (set_tile "retracted_distance" retracted_distance))
    ;;;设定【贝雷梁间距及数量】初始值
    (if (= BaileyBeam_distance nil) (set_tile "BaileyBeam_distance" "例450*3+900*2+450*3") (set_tile "BaileyBeam_distance" BaileyBeam_distance))
    ;;;设定【贝雷梁纵桥向布置】初始值
    (if (= BaileyBeam_span nil) (set_tile "BaileyBeam_span" "例1500+3000*7+1500") (set_tile "BaileyBeam_span" BaileyBeam_span))
    ;;;设定【钢管桩直径】初始值
    (if (= steelpile_diameter nil) (set_tile "steelpile_diameter" "820") (set_tile "steelpile_diameter" steelpile_diameter))
    ;;;设定【钢管桩壁厚】初始值
    (if (= steelpile_thickness nil) (set_tile "steelpile_thickness" "10") (set_tile "steelpile_thickness" steelpile_thickness))
    ;;;设定【钢管桩桩长】初始值
    (if (= steelpile_length nil) (set_tile "steelpile_length" "例20000") (set_tile "steelpile_length" steelpile_length))
    ;;;设定【钢管桩横桥向间距】初始值
    (if (= steelpile_lateral_distance nil) (set_tile "steelpile_lateral_distance" "例3000+3000") (set_tile "steelpile_lateral_distance" steelpile_lateral_distance))
    ;;;设定【钢管桩横桥向管桩中心偏移】初始值
    (if (= skew_distance nil) (set_tile "skew_distance" "0") (set_tile "skew_distance" skew_distance))
    ;;;设定【钢管桩纵桥向桩顶间距】初始值
    (if (= steelpile_lengthways_distance1 nil) (set_tile "steelpile_lengthways_distance1" "例12000+3000+12000") (set_tile "steelpile_lengthways_distance1" steelpile_lengthways_distance1))
    ;;;设定【钢管桩纵桥向桩底间距】初始值
    (if (= steelpile_lengthways_distance2 nil) (set_tile "steelpile_lengthways_distance2" "例12000+3000+12000") (set_tile "steelpile_lengthways_distance2" steelpile_lengthways_distance2))
    ;;;设定【贝雷左端悬出】初始值
    (if (= leftout_distance nil) (set_tile "leftout_distance" "例0") (set_tile "leftout_distance" leftout_distance))
    ;;;设定【贝雷右端悬出】初始值
    (if (= rightout_distance nil) (set_tile "rightout_distance" "例0") (set_tile "rightout_distance" rightout_distance))
;;;初始化按钮
    ;;;生成mct文件
    (action_tile "ok"  "(mctoutput)")
    ;;;暂存
    (action_tile "save"  "(save_dialog)")
    ;;;退出
    (action_tile "exit" "(done_dialog 0)")
    ;;;选择截面
    (action_tile "selectsection" "(done_dialog 2)")
    ;;;重置编号
;;;    (action_tile "resetid" "(resetid)")
    ;;;自动从mct中识别编号，未完成！
;;;    (mode_tile "automaticreadid" 1)
;;;初始化单选按钮
;;;设定截面类型单选按钮,第一次启动默认按单型钢界面
;;;设置关键字为"rib_single_section"(单截面)的控件的值为1
;;;    (set_tile "rib_single_section" "1")
;;; 当用户还没有点击任何单选按钮时，此时ribvaluea/b均还没有生成，点击暂存后截面下拉列表的值rib_section_name不为空，运行if时需要ribvaluea的值进行判断，dcl文件中控件的值为string
;;; 如果不设置ribvaluea的初始，会导致变量为空的错误发生
    (if (= ribvaluea nil) (setq ribvaluea "1"))
    (if (= ribvalueb nil) (setq ribvalueb "0"))
;;;当单击小肋单截面按钮时，将双截面按钮控件的key设置为"0"，打开key为"rib_section_list"(小肋截面下拉列表)的控件，把rib_section_name_lst1(小肋单截面名字库(list))表中每一项依次添加进下拉列表中，关闭对下拉列表的操作
;;;下面开始对按钮赋值的操作，当单击小肋单截面按钮时，把下拉列表的value设置成rib_section_name1(小肋单截面下拉列表选中的表项序号(string))
;;;即用户在选择完单双截面后并选择下拉表中的一个截面后，该表项序号就会赋值给下拉列表，实现当用户点击按钮时，保留上一次的序号信息，该表项序号初始值是0
;;;ribvaluea/b是单双截面单选按钮控件的key，再设置一遍是为了实现用户选择截面点击暂存后也可以保留上次选择，print是为了检测，该变量需初始值，即用户未点击按钮就暂存时，变量为空，默认状态为a为1，b为0，当用户点击按钮再暂存，该变量被储存
    (action_tile "rib_single_section"
                 "(set_tile \"rib_double_section\" \"0\")
                  (start_list \"rib_section_list\")
                  (mapcar 'add_list rib_section_name_lst1)
                  (end_list)
                  (set_tile \"rib_section_list\" rib_section_name1)
                  (setq ribvaluea (get_tile \"rib_single_section\"))
                  (setq ribvalueb (get_tile \"rib_double_section\"))
                  (print ribvaluea)
                  (print ribvalueb)"
    )
;;;点击双截面按钮同上
    (action_tile "rib_double_section"
                 "(set_tile \"rib_single_section\" \"0\")
                  (start_list \"rib_section_list\")
                  (mapcar 'add_list rib_section_name_lst2)
                  (end_list)
                  (set_tile \"rib_section_list\" rib_section_name2)
                  (setq ribvaluea (get_tile \"rib_single_section\"))
                  (setq ribvalueb (get_tile \"rib_double_section\"))
                  (print ribvaluea)
                  (print ribvalueb)"
    )
    ;点击单截面按钮，双截面按钮的值变为0，打开关键字为rib_section_list(截面类型)的下拉列表，依次将rib_section_name_lst1中的项加入到截面类型表中，结束表处理
    ;点击双截面按钮，单截面按钮的值变为0，打开关键字为rib_section_list(截面类型)的下拉列表，依次将rib_section_name_lst2中的项加入到截面类型表中，结束表处理
    ;点击单截面按钮后，双截面按钮不可用，截面类型下拉列表呈现所有单截面的名字列表
    ;点击双截面按钮后，单截面按钮不可用，截面类型下拉列表呈现所有双截面的名字列表
;;;初始化下拉菜单
    (rib_sections)
    (setq rib_section_name_lst1 nil rib_section_name_lst2 nil)
    (foreach x rib_section1 (setq rib_section_name_lst1 (append rib_section_name_lst1 (list (nth 1 (StringRegExpS "[^,， ]+" x ""))))))
    ;rib-section1是单截面数据库，将rib-section1中的每一项都赋值给x代入到表达式中，表达式的功能是将字符串中的第2项(截面的名字)依次添加到rib_section_name_lst1中，rib_section_name_lst1是表
    (foreach x rib_section2 (setq rib_section_name_lst2 (append rib_section_name_lst2 (list (nth 1 (StringRegExpS "[^,， ]+" x ""))))))
    ;rib-section2是双截面数据库，将rib-section2中的每一项都赋值给x代入到表达式中，表达式的功能是将字符串中的第2项(截面的名字)依次添加到rib_section_name_lst2中，rib_section_name_lst2是表
;;;首先判断用户是否暂存，如果是第一次打开界面，则没有按下暂存键，rib_section_name=nil，设置按钮初始状态，将默认单截面信息导入
;;;当用户摁下暂存后，判断单截面按钮的值是0还是1，如果是1，说明上次选择是单截面，加载一遍单截面列表，加载一遍按钮value，把暂存后的rib_section_name(点击暂存后小肋下拉列表的值)放到单截面列表中
;;;当用户摁下暂存后，判断单截面按钮的值是0还是1，如果是0，说明上次选择是双截面，加载一遍双截面列表，加载一遍按钮value，把暂存后的rib_section_name放到双截面列表中
;;;小肋下拉列表有两个值，一个是选择单双截面后通过$value得到的值，该值用于存储用户在界面操作时的选择，另一个是通过save_dialog中(setq rib_section_name (get_tile "rib_section_list"))命令保存用户暂存后的下拉列表的值
    (if (= rib_section_name nil)
      (progn
	(set_tile "rib_single_section" "1")
	(set_tile "rib_double_section" "0")
	(start_list "rib_section_list");打开关键字为rib_section_list(截面类型)的下拉列表
	(mapcar 'add_list rib_section_name_lst1);将rib_section_name_lst1的表项逐个添加到rib_section_list(截面类型)的下拉列表中
	(end_list);结束对rib_section_list(截面类型)下拉列表的表处理
      )
      (progn
	(if (= (atoi ribvaluea) 1)
	  (progn
	    (start_list "rib_section_list")
	    (mapcar 'add_list rib_section_name_lst1)
	    (end_list)
	    (set_tile "rib_single_section" "1")
	    (set_tile "rib_double_section" "0")
	    (set_tile "rib_section_list" rib_section_name)
	  )
	)
	(if (= (atoi ribvaluea) 0)
	  (progn
	    (start_list "rib_section_list")
	    (mapcar 'add_list rib_section_name_lst2)
	    (end_list)
	    (set_tile "rib_single_section" "0")
	    (set_tile "rib_double_section" "1")
	    (set_tile "rib_section_list" rib_section_name)
	  )
	)
      )
    );用户点击暂存后保留用户选择
;通过rib_sections截面库和StringRegExpS正则表达式函数得到rib_section_name_lst1(单截面类型的名字)和rib_section_name_lst2(双截面类型的名字)
;没有点击下拉列表时，不执行(setq rib_section_name1 $value)命令，表项序号为空，先赋值默认初始值"0"，避免不点击下拉列表时该变量为空影响后续操作的情况
    (if (= rib_section_name1 nil) (setq rib_section_name1 "0"))
    (if (= rib_section_name2 nil) (setq rib_section_name2 "0"))
;;;
    (action_tile "rib_section_list" "(if (and (= (get_tile \"rib_single_section\") \"1\") (= (get_tile \"rib_double_section\") \"0\"))
                                       (progn
                                         (setq rib_section_name1 $value)
                                         (setq rib_single_section_name (nth (atoi rib_section_name1) rib_section_name_lst1))
                                         (print rib_section_name1)
                                         (print rib_single_section_name)
                                       )
                                       (progn
                                         (setq rib_section_name2 $value)
                                         (setq rib_double_section_name (nth (atoi rib_section_name2) rib_section_name_lst2))
                                         (print rib_section_name2)
                                         (print rib_double_section_name)
                                       )
                                     )"
    )
;;; 调用distributed_chosen函数生成用户选择分配梁截面命令
    (distributed_chosen)
;;;    (set_tile \"rib_section_list\" rib_section_name)
;;;初始化面板
    (setq sdt (start_dialog))
    (cond ((= sdt 1)
	   (mctoutput xlst outputlst)
;;;	   (setq nodeID (1+ nodeID) elementID (1+ elementID) sectionID (1+ sectionID) materialID (1+ materialID) beamloadID (1+ beamloadID))
	   )
	  ((= sdt 0)
	   (unload_dialog dcl_re)
	  )
	  ((= sdt 2)
	   (select_concrete_section)
	  )
    )
    (unload_dialog dcl_re)
    (vl-file-delete dclname)
  )
)


;保存对话框用户输入的内容，get-tile函数获取关键字(key)为"..."的控件的值
(defun save_dialog ()
;;;  (setq nodeID (get_tile "nodeID"));节点号
;;;  (setq elementID (get_tile "elementID"));单元号
;;;  (setq sectionID (get_tile "sectionID"));截面号
;;;  (setq materialID (get_tile "materialID"));材料号
  (setq rib_section_name (get_tile "rib_section_list"))    ;大肋截面型号 编号
  (setq rib_single_section (get_tile "rib_single_section"));大肋截面类型（单截面）
  (setq rib_double_section (get_tile "rib_double_section"));大肋截面类型（双拼截面）
  (setq rib_spacing_distance (get_tile "rib_spacing_distance"));横肋间距及数量(mm)例300*5+500+200*2
  (setq concretedensity (get_tile "concretedensity"));混凝土容重
  (setq construction_load (get_tile "construction_load"));施工荷载
  (setq template_load (get_tile "template_load"));模板荷载
  (setq relative_distance1 (get_tile "relative_distance1"));模板横肋超出梁截面外边缘(施工荷载加载范围，mm)
  (setq overhang_distance (get_tile "overhang_distance"));分配梁相对贝雷悬出
  (setq retracted_distance (get_tile "retracted_distance"));管桩相对分配梁缩进
  (setq BaileyBeam_distance (get_tile "BaileyBeam_distance"));贝雷梁间距及数量
  (setq BaileyBeam_span (get_tile "BaileyBeam_span"));贝雷梁纵桥向布置
  (setq distributed_section_list (get_tile "distributed_section_list"));分配梁类型
  (setq distributed_single_section (get_tile "distributed_single_section"));分配梁单截面
  (setq distributed_double_section (get_tile "distributed_double_section"));分配梁双截面
  (setq steelpile_diameter (get_tile "steelpile_diameter"));钢管桩直径
  (setq steelpile_thickness (get_tile "steelpile_thickness"));钢管桩壁厚
  (setq steelpile_length (get_tile "steelpile_length"));钢管桩桩长
  (setq steelpile_lateral_distance (get_tile "steelpile_lateral_distance"));钢管桩横向间距
  (setq skew_distance (get_tile "skew_distance"));钢管桩中心偏移
  (setq steelpile_lengthways_distance1 (get_tile "steelpile_lengthways_distance1"));钢管桩桩顶间距
  (setq steelpile_lengthways_distance2 (get_tile "steelpile_lengthways_distance2"));钢管桩桩底间距
  (setq leftout_distance (get_tile "leftout_distance"));贝雷左端悬出
  (setq rightout_distance (get_tile "rightout_distance"));贝雷右端悬出
)


;;;重置所有编号初始值
;;;(defun resetid ()
;;;  (set_tile "nodeID" "1");设置关键字(key)为nodeID的控件的值为1
;;;  (set_tile "elementID" "1")
;;;  (set_tile "sectionID" "1")
;;;  (set_tile "materialID" "1")
;;;  (setq nodeID "1"
;;;	elementID "1"
;;;	sectionID "1"
;;;	materialID "1"
;;;  );设置变量赋值为1
;;;)


;;;定义一个根据单双截面按钮选分配梁截面的函数
(defun distributed_chosen()
;;;分配梁单截面名称表和双截面名称表，先设置为空表
  (setq distributed_single_section_namelist nil
	distributed_double_section_namelist nil
  )
;;;导入分配梁截面库并利用正则表达式函数将单截面库distributed_section1和双截面库distributed_section2中的截面名称分别写进分配梁单截面名称表和双截面名称表
;;;然后先将双截面的信息导入到下拉列表中，不然打开操作界面时是空表不好看
  (distributed_sections)
  (foreach x distributed_section1 (setq distributed_single_section_namelist (append distributed_single_section_namelist (list (nth 1 (StringRegExpS "[^,， ]+" x ""))))))
  (foreach x distributed_section2 (setq distributed_double_section_namelist (append distributed_double_section_namelist (list (nth 1 (StringRegExpS "[^,， ]+" x ""))))))
;;;如果用户点击了暂存，将暂存结果保留显示，distributed_section_list是点击暂存后对下拉列表的赋值，该值又由distributed_single/double_section_number而来
;;;distributed_single/double_section_number是对应单双截面下拉列表的value(表项)
  (if (= distributedvaluea nil) (setq distributedvaluea "1"))
  (if (= distributedvalueb nil) (setq distributedvalueb "0"))
  (if (= distributed_section_list nil)
    (progn
      (set_tile "distributed_single_section" "0")
      (set_tile "distributed_double_section" "1")
      (start_list "distributed_section_list")
      (mapcar 'add_list distributed_double_section_namelist)
      (end_list)
    )
    (progn
      (if (= (atoi distributedvaluea) 1)
	(progn
	  (start_list "distributed_section_list")
	  (mapcar 'add_list distributed_double_section_namelist)
	  (end_list)
	  (set_tile "distributed_double_section" "1")
	  (set_tile "distributed_single_section" "0")
	  (set_tile "distributed_section_list" distributed_section_list)
	)
      )
      (if (= (atoi distributedvaluea) 0)
	(progn
	  (start_list "distributed_section_list")
	  (mapcar 'add_list distributed_single_section_namelist)
	  (end_list)
	  (set_tile "distributed_double_section" "0")
	  (set_tile "distributed_single_section" "1")
	  (set_tile "distributed_section_list" distributed_section_list)
	)
      )
    )
  )
;;;有了名称表之后根据互锁行按钮的选择，将对应的截面名称表加到分配梁截面下拉列表中
;;;当点击其中一个单选按钮时，另一个单选按钮不可用，将对应截面信息导入到下拉列表中
  (action_tile "distributed_double_section"
    "(set_tile \"distributed_single_section\" \"0\")
     (start_list \"distributed_section_list\")
     (mapcar 'add_list distributed_double_section_namelist)
     (end_list)
     (set_tile \"distributed_section_list\" distributed_double_section_number)
     (setq distributedvaluea (get_tile \"distributed_double_section\"))
     (setq distributedvalueb (get_tile \"distributed_single_section\"))"
  )
  (action_tile "distributed_single_section"
    "(set_tile \"distributed_double_section\" \"0\")
     (start_list \"distributed_section_list\")
     (mapcar 'add_list distributed_single_section_namelist)
     (end_list)
     (set_tile \"distributed_section_list\" distributed_single_section_number)
     (setq distributedvaluea (get_tile \"distributed_double_section\"))
     (setq distributedvalueb (get_tile \"distributed_single_section\"))"
  )
;;;为选中的表的表项赋值,得到用户选择的截面的表项序号和截面的信息，distributed_single/double_section_number是字符串
  (if (= distributed_single_section_number nil ) (setq distributed_single_section_number "0"))
  (if (= distributed_double_section_number nil ) (setq distributed_double_section_number "0"))
  (action_tile "distributed_section_list" "(if (and (= (get_tile \"distributed_double_section\") \"1\") (= (get_tile \"distributed_single_section\") \"0\"))
                                             (progn
                                               (setq distributed_double_section_number $value)
                                               (setq distributed_double_section_fill (nth (atoi distributed_double_section_number) distributed_double_section_namelist))
                                               (print distributed_double_section_number)
                                               (print distributed_double_section_fill)
                                             )
                                             (progn
                                               (setq distributed_single_section_number $value)
                                               (setq distributed_single_section_fill (nth (atoi distributed_single_section_number) distributed_single_section_namelist))
                                               (print distributed_single_section_number)
                                               (print distributed_single_section_fill)
                                             )
                                           )"
  )
  
)


;;;存储用户选择的各种截面的高和宽用于确定节点x/y/z轴的间距
(defun section_data()
;;;小肋的截面高度和宽度
;;;  (setq rib_section_chosen "1, DBUSER,I12.6,CC,0,0,0,0,0,0,YES,NO,H,2,126,74,5,8.4,0,0,7,0,0,0");测试，结果返回"126"，属性为字符串
  (setq rib_height (atoi (nth 14 (StringRegExpS "[^,， ]+" rib_section_chosen ""))))
  (setq rib_width (atoi (nth 15 (StringRegExpS "[^,， ]+" rib_section_chosen ""))))
;;;贝雷梁高度
  (setq BaileyBeam_height 1500)
;;;分配梁高度
  (setq distributed_height (atoi (nth 14 (StringRegExpS "[^,， ]+" distributed_section_chosen ""))))
)
  
  