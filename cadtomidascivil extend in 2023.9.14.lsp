;С�ߵ�Ԫ����(xl��rib��׺����С���йر���)
;С�ߵĽڵ㣺�����˵�(�ɽ����Ⱦ���)
;            ѡȡ��������ɵ�xlst��(����������߶ȱ仯����Ӧ�ĵ�)
;            �ͱ������ĵ������ӵ�
;��select_concrete_section��������ȡ��Ч������xlst\hightlst
;xlst�ǽ������߶˵���е���ɵĵ��hightlst�������������ߵ���Ч�߶�
;xl_beamload������������С�߽ڵ㡢��Ԫ�����ϵĻ�����/ʩ��/ģ�����
(defun xl_beamload ()
;;;  �õ�����߽�����������������,����xlst_pt_begin��xlst���ĵ�һ���㣬xlst_pt_end��xlst�������һ��  
;;;  (setq xlst (list -5 -3 -1 1 3 5 7 9 11 13 15));����
  (setq xlst_pt_begin (nth 0 xlst)
	xlst_pt_end (nth 0 (reverse xlst))
  )
;;;  (setq xlst_pt_lst (list (list xlst_pt_begin 0 0) (list xlst_pt_end 0 0)));���ԣ�����((-5 0 0) (15 0 0))
;;;  �����������ָ���С�ߵ���β���ټӼ�ģ����߳������������Ե�������ģ����С�ߵ������յ��y����(midas�е�ֱ������ϵ)
;;;  ��save_dialog�������趨��(setq relative_distance1 (get_tile "relative_distance1"))��relative_distance1�ڿؼ�value�е��������ַ���
;;;  (setq relative_distance1 "15");����
  (setq relative_distance1_atof (atof relative_distance1))
  (setq xl_pt_begin (- xlst_pt_begin relative_distance1_atof)
	xl_pt_end (+ xlst_pt_end relative_distance1_atof)
  )
;;;  (princ xlst_pt_lst)
;;;  (princ "\n")
;;;  (princ xl_pt_begin)
;;;  (princ "\n")
;;;  (princ xl_pt_end)
;;;  (princ);���ԣ�����((-5 0 0) (15 0 0)) -20.0 30.0
;;;  xl_pt_list���õ�С���ϻ��������ص�������˵��y����(midas�е�ֱ������ϵ)
  (setq xl_pt_list1 (append (list xl_pt_begin) xlst (list xl_pt_end)))
;;;  ��ʼ������Ԫ,nodeid��save_dialog�������趨��(setq nodeID (get_tile "nodeID"))��nodeid�ڿؼ�value�е��������ַ�������ʼ����"1"
  (setq i 0
	nodes nil
	times (length xl_pt_list1)
	elements nil
	loads nil
  )
;;;  �û�����С�߼���ʹ��StringRegExpS��midasdisttolst1��������(300 300 300 300 300 500 200 200)��ʽ�ı�������߼�༰����(mm)��key��rib_spacing_distance
  (setq rib_spacing_distance_list
	 (midasdisttolst1 (get_tile "rib_spacing_distance"))
  )
;;;  �õ�(300 300 300 300 300 500 200 200)��ʽ�ı����ȡ����е�Ԫ�ظ��������ݸ�����ȷ��С�ߵĸ�����Ȼ��ʼѭ��
;;;  ��ѭ����ѭ������С�ߵ�Ԫ��Сѭ����ѭ������һ��С�ߵĽڵ㣬��һ��С�ߵ�x����Ϊ0����������չ
;;;  ��һ��ģ��С��(��Ϊx,z=0)��y������xl_pt_begin/xlst/xl_pt_end���������
;;;  rib_quantity��С��������rib_coordinate_x��С�ߺ����꣬nodestring�ǽڵ���
  (setq rib_quantity (1+ (length rib_spacing_distance_list)))
  (setq rib_coordinate_x 0
	rib_codinate_x_i 0
  )
  (repeat rib_quantity;��ʼ��һ��С�ߵ�Ԫ����
    (setq startnodeid (atoi nodeid))
    (repeat times;��ʼ��һ��С�߽ڵ�����
      (setq nodestring (strcat nodeid "," (itoa rib_coordinate_x) "," (rtos (nth i xl_pt_list1) 2 8) ",0"))
      (setq nodeid (itoa (+ 1 (atoi nodeid))))
      (setq nodes (append nodes (list nodestring)))
      (setq i (+ 1 i))
    )
;;; nodes����("1,0,y1,0" "2,0,y2,0" "3,0,y3,0" ... "n,0,yn,0")������y0��yn��xl_pt_list1���е�ԭ��
;;; ���˵�һ��С��Ŀ��ڵ��������ɣ�������������Щ��ɵ�Ԫ��С�߲��Ϻźͽ���ž�Ϊ1��elementid��save_dialog�������趨��(setq elementID (get_tile "elementID"))����ʼ����"1"
    (repeat (- times 1)
      (setq elementstring (strcat elementid ",BEAM,1,1," (itoa startnodeid) "," (itoa (+ 1 startnodeid)) ",0,0"))
      (setq elementid (itoa (+ 1 (atoi elementid))))
      (setq elements (append elements (list elementstring)))
      (setq startnodeid (+ 1 startnodeid))
    )
;;; ����times=5����һ��С�߽ڵ���5������Ԫ��4����startnodeid�ĳ�ʼֵΪ1
;;; elements����("1,BEAM,1,1,1,2,0,0" "2,BEAM,1,1,2,3,0,0" "3,BEAM,1,1,3,4,0,0" "4,BEAM,1,1,4,5,0,0")
;;; rib_coordinate_x����ȷ��С�ߺ����꣬ÿ��ѭ�������ӣ�append (list 0)����ΪС��������ѭ��������С�߼���ѭ��������1����0��Ӱ�����һ��С�ߵ�x���겢��ѭ��������Ӧ
    (setq rib_coordinate_x (+ rib_coordinate_x (nth rib_codinate_x_i (append rib_spacing_distance_list (list 0)))))
    (if (<= rib_codinate_x_i (- rib_quantity 2));��2����Ϊ����Ŵ�0��ʼ��repeat���Ǳ���ԭ����Ŀ�����߲�2
     (setq rib_codinate_x_i (1+ rib_codinate_x_i))
    )
;;; ��Ԫ���ɺ���ӻ�������ʩ��ģ����أ����￼��ֱ�߶β�������������
;;; ������������С�߼�ࡢ������Ч�߶ȡ���λ����ȷ��
  );(repeat rib_quantity ())����,����С�߽ڵ�͵�Ԫ����
)


;������ʽ������
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


;�ú���midasdisttolst1��������
;(setq temp2 (append temp2 (list "1")))
;(repeat temp4 (setq temp5 (append temp5 (list temp3))))
(defun midasdisttolst1 (txt / temp1 temp2 temp3 temp4 temp5)
  (setq temp1 (StringRegExpS "[^,��+ ]+" txt ""))
  (setq temp5 nil)
  (foreach x temp1
    (setq temp2 (StringRegExpS "[^��@*]+" x ""))
    (if	(= (length temp2) 1)
      (setq temp2 (append temp2 (list "1")))
    )
    (setq temp3 (atoi (car temp2)))
    (setq temp4 (atoi (last temp2)))
    (repeat temp4 (setq temp5 (append temp5 (list temp3))))
  )
  temp5
)


;��������ʽ���в���
;;;(defun c:test1 ()
;;;  (setq txt "300*5+500+200*2")
;;;  (setq temp1 (StringRegExpS "[^,��+ ]+" txt ""))
;;;)
;����("300*5" "500" "200*2")
;;;(defun c:test2 ()
;;;  (setq list1 (midasdisttolst1 "300*5+500+200*2"))
;;;  (setq list2 (midasdisttolst1 "450*3+900*2+450*3"))
;;;  (setq list3 (midasdisttolst1 "1500+3000*7+1500"))
;;;  (setq list4 (midasdisttolst1 "3000+3000"))
;;;  (setq list5 (midasdisttolst1 "5000"))
;;;  (setq list6 (midasdisttolst1 "12000+3000+12000"))
;;;)
;���ؽ������
;;;(300 300 300 300 300 500 200 200)
;;;(450 450 450 900 900 450 450 450)
;;;(1500 3000 3000 3000 3000 3000 3000 3000 1500)
;;;(3000 3000)
;;;(5000)
;;;(12000 3000 12000)
;���Խ���


;;;����С�߽���⣬rib��С�ߣ�sections�����н��棬section1�������棬section2��˫����
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


;;;��������������⣬distributed����������sections�����н��棬section1�������棬section2��˫����
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


;;;�����ֹ�׮�����(���û��Լ����룬��������)��steelpile���ֹ�׮��section������
;;;steelpile_section������support_section_list�������Ѿ����õ���
;;;(defun steelpile_sections()
;;;  (setq steelpile_section
;;;	 '("DBUSER,�ֹ�׮��630x8,CC,0,0,0,0,0,0,YES,NO,P,2,630,8,0,0,0,0,0,0,0,0"
;;;	   "DBUSER,�ֹ�׮��820x10,CC,0,0,0,0,0,0,YES,NO,P,2,820,10,0,0,0,0,0,0,0,0"
;;;	  )
;;;  )
;;;)


;;;�����ֹ�׮����ϵ�����(�Ȳ���)��bracing������ϵ��sections�����н��棬section1�������棬section2��˫����
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


;;;�ֹܱ�����֧�ܽ����(�����û��ڱ�����ѡ��������һ�Ž���������ÿһ������ַ���)
;;;1-С�߽��棻2-�������Ҹˣ�3-���������ˣ�4-������б�ˣ�5-֧�ż��Ҹ����ˣ�6-֧�ż�б�ˣ�7-���������棻8-�ֹ�׮���棻9-�ֹ�׮����ϵ2C20a�Ҹˣ�10-�ֹ�׮����ϵC20a����б��
;;;����support_section_chosen�洢�û�ѡ���̶����õĽ�����Ϣ��support��֧�ܣ�section�����棬chosen����ѡ��
;;;����10��sec����һһ��Ӧÿ��������ŷ����������ֱ��ȡ�ú��޸ģ�sec����section��д
(defun support_section_list()
  (setq sec1 1 sec2 2 sec3 3 sec4 4 sec5 5 sec6 6 sec7 7 sec8 8 sec9 9 sec10 10)
;;;д��С�߽���,���ؼ�rib_double_section��ֵΪ0ʱ��˵����ѡ�˵����水ť
;;;"rib_double_section"���ؼ�С��˫���水ť��key��rib_section_chosen���û�ѡ���С�߽�����(string)
;;;rib_section_name1��С�߽��������б���С�ߵ�����ı������(string)��rib_section1��С�ߵ������(list)
;;;rib_section_name2��С�߽��������б���С��˫����ı������(string)��rib_section2��С��˫�����(list)
  (if (= (get_tile "rib_double_section") 0)
      (progn
	(setq rib_section_chosen (strcat (itoa sec1) ", " (nth (atoi rib_section_name1) rib_section1)))
      )
      (progn
	(setq rib_section_chosen (strcat (itoa sec1) ", " (nth (atoi rib_section_name2) rib_section2)))
      )
  )
;;;д�뱴������֧�żܽ���
;;;Bailey����������section1/2/3�����ֶ�Ӧ���棬L ��L�͸�(֧�ż�)
  (setq Bailey_section1 (strcat (itoa sec2) ", " "DBUSER, �������Ҹ�, CC, 0, 0, 0, 0, 0, 0, YES, NO, 2C , 2, 100, 48, 5.3, 8.5, 80, 0, 0, 0, 0, 0"))
  (setq Bailey_section2 (strcat (itoa sec3) ", " "DBUSER, ����������, CC, 0, 0, 0, 0, 0, 0, YES, NO, H , 2, 80, 50, 4.5, 6.5, 0, 0, 0, 0, 0, 0"))
  (setq Bailey_section3 (strcat (itoa sec4) ", " "DBUSER, ������б��, CC, 0, 0, 0, 0, 0, 0, YES, NO, H , 2, 80, 50, 4.5, 6.5, 0, 0, 0, 0, 0, 0"))
  (setq L_section1 (strcat (itoa sec5) ", " "DBUSER, L 75x50x6, CC, 0, 0, 0, 0, 0, 0, YES, NO, L , 2, 75, 50, 6, 6, 0, 0, 0, 0, 0, 0"))
  (setq L-section2 (strcat (itoa sec6) ", " "DBUSER, L 40x5, CC, 0, 0, 0, 0, 0, 0, YES, NO, L , 1, GB-YB05, L 40x5"))
;;;д����������棬���ؼ�distributed_single_section��ֵΪ0ʱ��˵���û�ѡ����˫����
;;;"distributed_single_section"���ؼ������������水ť��key��distributed_section_chosen���û�ѡ��ķ�����������(string)
;;;distributed_single_section_number�����������������б��е�����ı������(string)��distributed_section1���������������(list)
;;;distributed_double_section_number�����������������б���˫����ı������(string)��distributed_section2��������˫�����(list)
  (if (= (get_tile "distributed_single_section") 0)
      (progn
	(setq distributed_section_chosen (strcat (itoa sec7) ", " (nth (atoi distributed_double_section_number) distributed_section2)))
      )
      (progn
	(setq distributed_section_chosen (strcat (itoa sec7) ", " (nth (atoi distributed_single_section_number) distributed_section1)))
      )
  )
;;;д��ֹ�׮���棬�ý������û��������
;;;�ֹ�׮ֱ��steelpile_diameter��(get_tile "steelpile_diameter")���,"steelpile_diameter"��key������(string)
;;;�ֹ�׮�ں�steelpile_thickness��(get_tile "steelpile_thickness")��ã�"steelpile_thickness"��key������(string)
;;;(setq sec8 8 steelpile_diameter "820" steelpile_thickness "10");����
  (setq steelpile_section (strcat (itoa sec8) ", " "DBUSER, " "�ֹ�׮" steelpile_diameter "x" steelpile_thickness ", CC, 0, 0, 0, 0, 0, 0, YES, NO, P , 2, " steelpile_diameter ", " steelpile_thickness ", 0, 0, 0, 0, 0, 0, 0, 0"))
;;;д��ֹ�׮����ϵ����(����Ҳ�������û���һ����Χ���Լ�ѡ��)
;;;2C20a��˫C20a��C_20a������20a
  (setq 2C20a_section (strcat (itoa sec9) ", " "DBUSER, 2C20a, CC, 0, 0, 0, 0, 0, 0, YES, NO, 2C , 1, GB-YB05, 2C 20a")
	C_20a_section (strcat (itoa sec10) ", " "DBUSER, C 20a, CC, 0, 0, 0, 0, 0, 0, YES, NO, C  , 1, GB-YB05, C 20a")
  )
;;;�õ����н�����Ϣ��ʼ�����ܱ�
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
);����("1, ..." "2, ..." ... "10, ..." ...)������ÿһ��������Ϣ��Ӧ���ַ����Ǳ��е�һ��ԭ��


;;;(defun c:a1 ();����
;;;  (setq a (getint "\n����һ��������"))
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


;;;��������չ
;;;��γ�����lisp��ģ��dcl�ļ�����ʱ��Ҫһ����ʱ�ļ�����д������ļ�
(defun c:CTM (/ dclname tempname filen stream dcl_re)
  (setvar "cmdecho" 0)
  (setq sdt nil)
  (while (and (/= sdt 0) (/= sdt 1))
;;;sdt=0ж�ضԻ���sdt=1����mct�ļ�����ͬʱ����ʱ��˵���û�û����Ҫ�Ի��������ò���������ʱ�Ի����ļ���ʼ����
    (setq dclname
	   (cond
	     ((setq tempname (vl-filename-mktemp "cadtomidascivil.dcl");���ش���Ŀ¼�µĸ��ļ�·����ת��Ϊ�ַ���
		    filen    (open tempname "w");����tempname���ļ�������
	      )
	      (foreach stream;stream��Ϊ�β�ͨ��foreach������list��ÿһ���ַ�������д��
		    (list "\n"
			  "cadtomidascivil:dialog {                                                                                                    \n";���ɶԻ��򣬶Ի����������cadtomidascivil
			  " label=\"�ֹܱ�����֧�ܲ�������ģV0.1-����\";                                                                               \n";�Ի����ǩ"�Ǹֹܱ�����֧�ܲ�������ģV0.1-����"
;;;;;;			  �����ʼ���
;;;			  " :boxed_column { label=\"��ʼ��ţ�\";                                                                                      \n";�ӿ���1-��ǩ"��ʼ���"
;;;			  " :row {                                                                                                                     \n";�ӿ���1�ĵ�1��
;;;                          " :edit_box  { label=\"�ڵ�ţ�\"; key = \"nodeID\"; value=\"1\";}                                                           \n";�༭��-��ǩ"�ڵ��"-�ؼ���nodeID-ֵ1
;;;			  " :edit_box  { label=\"��Ԫ�ţ�\"; key = \"elementID\"; value=\"1\";}                                                        \n";�༭��-��ǩ"��Ԫ��"-�ؼ���elementID-ֵ1
;;;			  " :edit_box  { label=\"����ţ�\"; key = \"sectionID\"; value=\"1\";}                                                        \n";�༭��-��ǩ"�����"-�ؼ���sectionID-ֵ1
;;;			  "      }                                                                                                                     \n";�ӿ���1��1�н���
;;;			  " :row {                                                                                                                     \n";�ӿ���1�ĵ�2��
;;;			  " :edit_box  { label=\"���Ϻţ�\"; key = \"materialID\"; value=\"1\";}                                                       \n";�༭��-��ǩ"���Ϻ�"-�ؼ���materialID-ֵ1
;;;			  " :button { key=\"resetid\";label=\"����Ϊ1\"; }                                                                             \n";��ť-��ǩ"����Ϊ1"-�ؼ���resetid
;;;			  " :button { key=\"automaticreadid\";label=\"��mct���Զ�ʶ��\"; }                                                             \n";��ť-��ǩ"��mct���Զ�ʶ��"-�ؼ���automaticreadid
;;;			  "      }                                                                                                                     \n";�ӿ���1��2�н���
;;;			  "               }                                                                                                            \n";�ӿ���1����
;;;			  һ�����С�ߵĲ�������
			  " :boxed_column { label=\"С��ģ��������ã�\";                                                                              \n"
;;;			  1�����ģ�����
			  " :boxed_column { label=\"ģ����ߣ�\";                                                                                      \n"
		          " :row {                                                                                                                     \n"
			  " :popup_list { label=\"��������\"; key= \"rib_section_list\";width=15;fixed_width = true;}                                  \n"
		          " :radio_button { label=\"������\"; key=\"rib_single_section\";}                                                             \n"
		          " :radio_button { label=\"˫ƴ����\"; key=\"rib_double_section\";}                                                           \n"
		          "      }                                                                                                                     \n"
			  " :row {                                                                                                                     \n"
			  " :edit_box  { label=\"���߼�༰����(mm)��\"; key = \"rib_spacing_distance\";value=\"��300*5+500+200*2\";}                  \n"
			  "      }                                                                                                                     \n"
			  "               }                                                                                                            \n"
;;;			  2����ӻ�������ʩ��ģ�����
			  " :boxed_row { label=\"���������أ�\";                                                                                       \n"
			  " :edit_box  { label=\"����������(kN/m3)��\"; key = \"concretedensity\"; value=\"26.5\"; }                                   \n"
			  " :button { label=\"ѡ�����\"; key=\"selectsection\"; }                                                                     \n"
			  "            }                                                                                                               \n"
			  " :boxed_row { label=\"ʩ����ģ����أ�\";                                                                                   \n"
			  " :edit_box  { label=\"ʩ������(kN/m2)��\"; key = \"construction_load\"; value=\"2\"; }                                      \n"
			  " :edit_box  { label=\"ģ�����(kN/m2)��\"; key = \"template_load\"; value=\"2.5\"; }                                        \n"
			  "            }                                                                                                               \n"
;;;			  1��2����
			  "               }                                                                                                            \n"
;;;			  ������ӱ������ӿ���
			  " :boxed_column { label=\"�������������ã�\";                                                                                \n"
			  " :edit_box  { label=\"��������༰����(mm)��\"; key = \"BaileyBeam_distance\"; value=\"��450*3+900*2+450*3\";}              \n"
			  " :edit_box  { label=\"��������������(mm)��\"; key = \"BaileyBeam_span\"; value=\"��1500+3000*7+1500\";}                   \n"
			  "               }                                                                                                            \n"
;;;			  ������ӷ������ӿ���
			  " :boxed_row { label=\"�������������ã�\";                                                                                   \n"
			  " :popup_list { label=\"����������\"; key= \"distributed_section_list\";width=30;fixed_width = true;}                        \n"
			  " :radio_row {                                                                                                               \n"
			  "  key = \"distributed_section_button\";                                                                                     \n"
			  " :radio_button { label=\"������\"; key=\"distributed_single_section\"; mnemonic= \"0\"; value=0;}                           \n"
			  " :radio_button { label=\"˫����\"; key=\"distributed_double_section\"; mnemonic= \"1\"; value=1;}                           \n"
			  "            }                                                                                                               \n"
			  "            }                                                                                                               \n"
;;;			 �ġ� ��Ӹֹ�׮�ӿ���
			  " :boxed_column { label=\"�ֹ�׮�������ã�\";                                                                                \n"
			  " :boxed_row { label=\"�ֹ�׮���\";                                                                                       \n"
			  " :edit_box  { label=\"�ֹ�׮ֱ��(mm)��\"; key = \"steelpile_diameter\"; value=\"820\"; }                                    \n"
			  " :edit_box  { label=\"�ֹ�׮�ں�(mm)��\"; key = \"steelpile_thickness\"; value=\"10\"; }                                    \n"
			  " :edit_box  { label=\"׮��(mm)��\"; key = \"steelpile_length\"; value=\"��20000\"; }                                        \n"
			  "            }                                                                                                               \n"		  
			  " :row {                                                                                                                     \n"
			  " :boxed_column { label=\"������������ã�\";                                                                                \n"
			  " :row {                                                                                                                     \n"
			  " :edit_box  { label=\"���(mm)��\"; key = \"steelpile_lateral_distance\"; width=30;value=\"��3000+3000\"; }                 \n"
			  " :edit_box  { label=\"��׮����ƫ�ƣ�Ĭ�϶Գƾ���,mm����\"; key = \"skew_distance\"; value=\"0\"; }                          \n"
			  "      }                                                                                                                     \n"
			  "               }                                                                                                            \n"
			  "      }                                                                                                                     \n"
			  " :row {                                                                                                                     \n"
			  " :boxed_row { label=\"������������ã�\";                                                                                   \n"
			  " :column {                                                                                                                  \n"
			  " :edit_box  { label=\"׮�����(mm)��\"; key = \"steelpile_lengthways_distance1\"; value=\"��12000+3000+12000\"; }           \n"
			  " :edit_box  { label=\"׮�׼��(mm)��\"; key = \"steelpile_lengthways_distance2\"; value=\"��12000+3000+12000\"; }           \n"
			  "         }                                                                                                                  \n"
			  " :column {                                                                                                                  \n"
			  " :edit_box  { label=\"�����������(mm)��\"; key = \"leftout_distance\"; value=\"��0\"; }                                    \n"
			  " :edit_box  { label=\"�����Ҷ�����(mm)��\"; key = \"rightout_distance\"; value=\"��0\"; }                                   \n"
			  "         }                                                                                                                  \n"
			  "               }                                                                                                            \n"
			  "      }                                                                                                                     \n"
			  "               }                                                                                                            \n"
;;;			  �塢��ӽṹ���λ�ù�ϵ
			  " :boxed_column { label=\"���ṹ���λ�ù�ϵ��\";                                                                            \n"
			  " :edit_box  { label=\"ģ����߳������������Ե(ʩ�����ؼ��ط�Χ��mm)��\"; key = \"relative_distance1\"; value=\"1500\"; }   \n"
			  " :row {                                                                                                                     \n"
			  " :edit_box  { label=\"��������Ա�������(mm)��\"; key = \"overhang_distance\"; value=\"��1500\"; }                          \n"
			  " :edit_box  { label=\"��׮��Է���������(mm)��\"; key = \"retracted_distance\"; value=\"��1500\"; }                         \n"
			  "      }                                                                                                                     \n"
			  "               }                                                                                                            \n"
;;;			  ������ӵײ���ť
			  " :row {                                                                                                                     \n";��1
			  "          :button { key=\"ok\";label=\"����\"; }                                                                            \n";���ɰ�ť
			  "          :button { key=\"save\";label=\"�ݴ�\"; }                                                                          \n";�ݴ水ť
			  "          :button { key=\"exit\";label=\"�˳�\"; is_cancel=true; }                                                          \n";�˳���ť
			  "      }                                                                                                                     \n";��1����
			  "}                                                                                                                           \n";�Ի������
	        )
		(princ stream filen);(foreach stream (list �ַ���) (princ stream filename))
	      );foreach stream ...����
	      (close filen);�رն�Ӧ���ļ�
	      tempname
	     );cond��������ʱlist�е��ַ�����д�뵽dclname�У�ʵ��lispģ��dcl�Ի����ļ�
	   );setq dclname����
    );while (and (/= sdt 0) (/= sdt 1)����
    (setq dcl_re (load_dialog dclname));����dclname�Ի���(lispģ��dcl�ļ�)����ֵ��dcl_re
    (if	(not (new_dialog "cadtomidascivil" dcl_re));���û�д�dcl_re������Ϊcadtomidascivil�ĶԻ����ļ����˳�����
      (exit)
    )
;;;��ʼ���ı���
;;;���ID�����ڣ�����1Ϊ��ģ�ĳ�ʼ�ڵ㣬���ĳID�Ѿ����ڣ�����Ϊ��ʼID
;;;Ŀǰ��¼��ID��nodeID��elementID��sectionID��materialID������Ԫ����beamloadID�����thicknessID
;;;��ȱ�ṹ��groupID���߽���BNDR-groupID��������LOAD-groupID���ڵ����conloadID��һ��֧��constraintID ,��������elasticlinkID
;;;    ;;;�趨���ڵ㡿�����ʼֵ
;;;    (if (= nodeID nil) (set_tile "nodeID" "1") (set_tile "nodeID" nodeID))
;;;    ;;;�趨����Ԫ�������ʼֵ
;;;    (if (= elementID nil) (set_tile "elementID" "1") (set_tile "elementID" elementID))
;;;    ;;;�趨�����桿�����ʼֵ
;;;    (if (= sectionID nil) (set_tile "sectionID" "1") (set_tile "sectionID" sectionID))
;;;    ;;;�趨�����ϡ������ʼֵ
;;;    (if (= materialID nil) (set_tile "materialID" "1") (set_tile "materialID" materialID))
    ;;;�趨�����߼�༰��������ʼֵ
    (if (= rib_spacing_distance nil) (set_tile "rib_spacing_distance" "��300*5+500+200*2") (set_tile "rib_spacing_distance" rib_spacing_distance))
    ;;;�趨���������ضȡ���ʼֵ
    (if (= concretedensity nil) (set_tile "concretedensity" "26.5") (set_tile "concretedensity" concretedensity))
    ;;;�趨��ʩ�����ء���ʼֵ
    (if (= construction_load nil) (set_tile "construction_load" "2") (set_tile "construction_load" construction_load))
    ;;;�趨��ģ����ء���ʼֵ
    (if (= template_load nil) (set_tile "template_load" "2.5") (set_tile "template_load" template_load))
    ;;;�趨��ģ����߳������������Ե����ʼֵ
    (if (= relative_distance1 nil) (set_tile "relative_distance1" "1500") (set_tile "relative_distance1" relative_distance1))
    ;;;�趨����������Ա�����������ʼֵ
    (if (= overhang_distance nil) (set_tile "overhang_distance" "��1500") (set_tile "overhang_distance" overhang_distance))
    ;;;�趨����׮��Է�������������ʼֵ
    (if (= retracted_distance nil) (set_tile "retracted_distance" "��1500") (set_tile "retracted_distance" retracted_distance))
    ;;;�趨����������༰��������ʼֵ
    (if (= BaileyBeam_distance nil) (set_tile "BaileyBeam_distance" "��450*3+900*2+450*3") (set_tile "BaileyBeam_distance" BaileyBeam_distance))
    ;;;�趨���������������á���ʼֵ
    (if (= BaileyBeam_span nil) (set_tile "BaileyBeam_span" "��1500+3000*7+1500") (set_tile "BaileyBeam_span" BaileyBeam_span))
    ;;;�趨���ֹ�׮ֱ������ʼֵ
    (if (= steelpile_diameter nil) (set_tile "steelpile_diameter" "820") (set_tile "steelpile_diameter" steelpile_diameter))
    ;;;�趨���ֹ�׮�ں񡿳�ʼֵ
    (if (= steelpile_thickness nil) (set_tile "steelpile_thickness" "10") (set_tile "steelpile_thickness" steelpile_thickness))
    ;;;�趨���ֹ�׮׮������ʼֵ
    (if (= steelpile_length nil) (set_tile "steelpile_length" "��20000") (set_tile "steelpile_length" steelpile_length))
    ;;;�趨���ֹ�׮�������ࡿ��ʼֵ
    (if (= steelpile_lateral_distance nil) (set_tile "steelpile_lateral_distance" "��3000+3000") (set_tile "steelpile_lateral_distance" steelpile_lateral_distance))
    ;;;�趨���ֹ�׮�������׮����ƫ�ơ���ʼֵ
    (if (= skew_distance nil) (set_tile "skew_distance" "0") (set_tile "skew_distance" skew_distance))
    ;;;�趨���ֹ�׮������׮����ࡿ��ʼֵ
    (if (= steelpile_lengthways_distance1 nil) (set_tile "steelpile_lengthways_distance1" "��12000+3000+12000") (set_tile "steelpile_lengthways_distance1" steelpile_lengthways_distance1))
    ;;;�趨���ֹ�׮������׮�׼�ࡿ��ʼֵ
    (if (= steelpile_lengthways_distance2 nil) (set_tile "steelpile_lengthways_distance2" "��12000+3000+12000") (set_tile "steelpile_lengthways_distance2" steelpile_lengthways_distance2))
    ;;;�趨�����������������ʼֵ
    (if (= leftout_distance nil) (set_tile "leftout_distance" "��0") (set_tile "leftout_distance" leftout_distance))
    ;;;�趨�������Ҷ���������ʼֵ
    (if (= rightout_distance nil) (set_tile "rightout_distance" "��0") (set_tile "rightout_distance" rightout_distance))
;;;��ʼ����ť
    ;;;����mct�ļ�
    (action_tile "ok"  "(mctoutput)")
    ;;;�ݴ�
    (action_tile "save"  "(save_dialog)")
    ;;;�˳�
    (action_tile "exit" "(done_dialog 0)")
    ;;;ѡ�����
    (action_tile "selectsection" "(done_dialog 2)")
    ;;;���ñ��
;;;    (action_tile "resetid" "(resetid)")
    ;;;�Զ���mct��ʶ���ţ�δ��ɣ�
;;;    (mode_tile "automaticreadid" 1)
;;;��ʼ����ѡ��ť
;;;�趨�������͵�ѡ��ť,��һ������Ĭ�ϰ����͸ֽ���
;;;���ùؼ���Ϊ"rib_single_section"(������)�Ŀؼ���ֵΪ1
;;;    (set_tile "rib_single_section" "1")
;;; ���û���û�е���κε�ѡ��ťʱ����ʱribvaluea/b����û�����ɣ�����ݴ����������б��ֵrib_section_name��Ϊ�գ�����ifʱ��Ҫribvaluea��ֵ�����жϣ�dcl�ļ��пؼ���ֵΪstring
;;; ���������ribvaluea�ĳ�ʼ���ᵼ�±���Ϊ�յĴ�����
    (if (= ribvaluea nil) (setq ribvaluea "1"))
    (if (= ribvalueb nil) (setq ribvalueb "0"))
;;;������С�ߵ����水ťʱ����˫���水ť�ؼ���key����Ϊ"0"����keyΪ"rib_section_list"(С�߽��������б�)�Ŀؼ�����rib_section_name_lst1(С�ߵ��������ֿ�(list))����ÿһ��������ӽ������б��У��رն������б�Ĳ���
;;;���濪ʼ�԰�ť��ֵ�Ĳ�����������С�ߵ����水ťʱ���������б��value���ó�rib_section_name1(С�ߵ����������б�ѡ�еı������(string))
;;;���û���ѡ���굥˫�����ѡ���������е�һ������󣬸ñ�����žͻḳֵ�������б�ʵ�ֵ��û������ťʱ��������һ�ε������Ϣ���ñ�����ų�ʼֵ��0
;;;ribvaluea/b�ǵ�˫���浥ѡ��ť�ؼ���key��������һ����Ϊ��ʵ���û�ѡ��������ݴ��Ҳ���Ա����ϴ�ѡ��print��Ϊ�˼�⣬�ñ������ʼֵ�����û�δ�����ť���ݴ�ʱ������Ϊ�գ�Ĭ��״̬ΪaΪ1��bΪ0�����û������ť���ݴ棬�ñ���������
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
;;;���˫���水ťͬ��
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
    ;��������水ť��˫���水ť��ֵ��Ϊ0���򿪹ؼ���Ϊrib_section_list(��������)�������б����ν�rib_section_name_lst1�е�����뵽�������ͱ��У���������
    ;���˫���水ť�������水ť��ֵ��Ϊ0���򿪹ؼ���Ϊrib_section_list(��������)�������б����ν�rib_section_name_lst2�е�����뵽�������ͱ��У���������
    ;��������水ť��˫���水ť�����ã��������������б�������е�����������б�
    ;���˫���水ť�󣬵����水ť�����ã��������������б��������˫����������б�
;;;��ʼ�������˵�
    (rib_sections)
    (setq rib_section_name_lst1 nil rib_section_name_lst2 nil)
    (foreach x rib_section1 (setq rib_section_name_lst1 (append rib_section_name_lst1 (list (nth 1 (StringRegExpS "[^,�� ]+" x ""))))))
    ;rib-section1�ǵ��������ݿ⣬��rib-section1�е�ÿһ���ֵ��x���뵽���ʽ�У����ʽ�Ĺ����ǽ��ַ����еĵ�2��(���������)������ӵ�rib_section_name_lst1�У�rib_section_name_lst1�Ǳ�
    (foreach x rib_section2 (setq rib_section_name_lst2 (append rib_section_name_lst2 (list (nth 1 (StringRegExpS "[^,�� ]+" x ""))))))
    ;rib-section2��˫�������ݿ⣬��rib-section2�е�ÿһ���ֵ��x���뵽���ʽ�У����ʽ�Ĺ����ǽ��ַ����еĵ�2��(���������)������ӵ�rib_section_name_lst2�У�rib_section_name_lst2�Ǳ�
;;;�����ж��û��Ƿ��ݴ棬����ǵ�һ�δ򿪽��棬��û�а����ݴ����rib_section_name=nil�����ð�ť��ʼ״̬����Ĭ�ϵ�������Ϣ����
;;;���û������ݴ���жϵ����水ť��ֵ��0����1�������1��˵���ϴ�ѡ���ǵ����棬����һ�鵥�����б�����һ�鰴ťvalue�����ݴ���rib_section_name(����ݴ��С�������б��ֵ)�ŵ��������б���
;;;���û������ݴ���жϵ����水ť��ֵ��0����1�������0��˵���ϴ�ѡ����˫���棬����һ��˫�����б�����һ�鰴ťvalue�����ݴ���rib_section_name�ŵ�˫�����б���
;;;С�������б�������ֵ��һ����ѡ��˫�����ͨ��$value�õ���ֵ����ֵ���ڴ洢�û��ڽ������ʱ��ѡ����һ����ͨ��save_dialog��(setq rib_section_name (get_tile "rib_section_list"))������û��ݴ��������б��ֵ
    (if (= rib_section_name nil)
      (progn
	(set_tile "rib_single_section" "1")
	(set_tile "rib_double_section" "0")
	(start_list "rib_section_list");�򿪹ؼ���Ϊrib_section_list(��������)�������б�
	(mapcar 'add_list rib_section_name_lst1);��rib_section_name_lst1�ı��������ӵ�rib_section_list(��������)�������б���
	(end_list);������rib_section_list(��������)�����б�ı���
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
    );�û�����ݴ�����û�ѡ��
;ͨ��rib_sections������StringRegExpS������ʽ�����õ�rib_section_name_lst1(���������͵�����)��rib_section_name_lst2(˫�������͵�����)
;û�е�������б�ʱ����ִ��(setq rib_section_name1 $value)����������Ϊ�գ��ȸ�ֵĬ�ϳ�ʼֵ"0"�����ⲻ��������б�ʱ�ñ���Ϊ��Ӱ��������������
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
;;; ����distributed_chosen���������û�ѡ���������������
    (distributed_chosen)
;;;    (set_tile \"rib_section_list\" rib_section_name)
;;;��ʼ�����
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


;����Ի����û���������ݣ�get-tile������ȡ�ؼ���(key)Ϊ"..."�Ŀؼ���ֵ
(defun save_dialog ()
;;;  (setq nodeID (get_tile "nodeID"));�ڵ��
;;;  (setq elementID (get_tile "elementID"));��Ԫ��
;;;  (setq sectionID (get_tile "sectionID"));�����
;;;  (setq materialID (get_tile "materialID"));���Ϻ�
  (setq rib_section_name (get_tile "rib_section_list"))    ;���߽����ͺ� ���
  (setq rib_single_section (get_tile "rib_single_section"));���߽������ͣ������棩
  (setq rib_double_section (get_tile "rib_double_section"));���߽������ͣ�˫ƴ���棩
  (setq rib_spacing_distance (get_tile "rib_spacing_distance"));���߼�༰����(mm)��300*5+500+200*2
  (setq concretedensity (get_tile "concretedensity"));����������
  (setq construction_load (get_tile "construction_load"));ʩ������
  (setq template_load (get_tile "template_load"));ģ�����
  (setq relative_distance1 (get_tile "relative_distance1"));ģ����߳������������Ե(ʩ�����ؼ��ط�Χ��mm)
  (setq overhang_distance (get_tile "overhang_distance"));��������Ա�������
  (setq retracted_distance (get_tile "retracted_distance"));��׮��Է���������
  (setq BaileyBeam_distance (get_tile "BaileyBeam_distance"));��������༰����
  (setq BaileyBeam_span (get_tile "BaileyBeam_span"));��������������
  (setq distributed_section_list (get_tile "distributed_section_list"));����������
  (setq distributed_single_section (get_tile "distributed_single_section"));������������
  (setq distributed_double_section (get_tile "distributed_double_section"));������˫����
  (setq steelpile_diameter (get_tile "steelpile_diameter"));�ֹ�׮ֱ��
  (setq steelpile_thickness (get_tile "steelpile_thickness"));�ֹ�׮�ں�
  (setq steelpile_length (get_tile "steelpile_length"));�ֹ�׮׮��
  (setq steelpile_lateral_distance (get_tile "steelpile_lateral_distance"));�ֹ�׮������
  (setq skew_distance (get_tile "skew_distance"));�ֹ�׮����ƫ��
  (setq steelpile_lengthways_distance1 (get_tile "steelpile_lengthways_distance1"));�ֹ�׮׮�����
  (setq steelpile_lengthways_distance2 (get_tile "steelpile_lengthways_distance2"));�ֹ�׮׮�׼��
  (setq leftout_distance (get_tile "leftout_distance"));�����������
  (setq rightout_distance (get_tile "rightout_distance"));�����Ҷ�����
)


;;;�������б�ų�ʼֵ
;;;(defun resetid ()
;;;  (set_tile "nodeID" "1");���ùؼ���(key)ΪnodeID�Ŀؼ���ֵΪ1
;;;  (set_tile "elementID" "1")
;;;  (set_tile "sectionID" "1")
;;;  (set_tile "materialID" "1")
;;;  (setq nodeID "1"
;;;	elementID "1"
;;;	sectionID "1"
;;;	materialID "1"
;;;  );���ñ�����ֵΪ1
;;;)


;;;����һ�����ݵ�˫���水ťѡ����������ĺ���
(defun distributed_chosen()
;;;���������������Ʊ��˫�������Ʊ�������Ϊ�ձ�
  (setq distributed_single_section_namelist nil
	distributed_double_section_namelist nil
  )
;;;�������������Ⲣ����������ʽ�������������distributed_section1��˫�����distributed_section2�еĽ������Ʒֱ�д�����������������Ʊ��˫�������Ʊ�
;;;Ȼ���Ƚ�˫�������Ϣ���뵽�����б��У���Ȼ�򿪲�������ʱ�ǿձ��ÿ�
  (distributed_sections)
  (foreach x distributed_section1 (setq distributed_single_section_namelist (append distributed_single_section_namelist (list (nth 1 (StringRegExpS "[^,�� ]+" x ""))))))
  (foreach x distributed_section2 (setq distributed_double_section_namelist (append distributed_double_section_namelist (list (nth 1 (StringRegExpS "[^,�� ]+" x ""))))))
;;;����û�������ݴ棬���ݴ���������ʾ��distributed_section_list�ǵ���ݴ��������б�ĸ�ֵ����ֵ����distributed_single/double_section_number����
;;;distributed_single/double_section_number�Ƕ�Ӧ��˫���������б��value(����)
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
;;;�������Ʊ�֮����ݻ����а�ť��ѡ�񣬽���Ӧ�Ľ������Ʊ�ӵ����������������б���
;;;���������һ����ѡ��ťʱ����һ����ѡ��ť�����ã�����Ӧ������Ϣ���뵽�����б���
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
;;;Ϊѡ�еı�ı��ֵ,�õ��û�ѡ��Ľ���ı�����źͽ������Ϣ��distributed_single/double_section_number���ַ���
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


;;;�洢�û�ѡ��ĸ��ֽ���ĸߺͿ�����ȷ���ڵ�x/y/z��ļ��
(defun section_data()
;;;С�ߵĽ���߶ȺͿ��
;;;  (setq rib_section_chosen "1, DBUSER,I12.6,CC,0,0,0,0,0,0,YES,NO,H,2,126,74,5,8.4,0,0,7,0,0,0");���ԣ��������"126"������Ϊ�ַ���
  (setq rib_height (atoi (nth 14 (StringRegExpS "[^,�� ]+" rib_section_chosen ""))))
  (setq rib_width (atoi (nth 15 (StringRegExpS "[^,�� ]+" rib_section_chosen ""))))
;;;�������߶�
  (setq BaileyBeam_height 1500)
;;;�������߶�
  (setq distributed_height (atoi (nth 14 (StringRegExpS "[^,�� ]+" distributed_section_chosen ""))))
)
  
  