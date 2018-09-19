; by Mounica Reddy and Astha Sharma
; Project2: Expert Academic Advisor

; The expert system will advise students regarding what course the student should select for their
;Masters studies. The expert system will gather information like undergraduate Score, academic
;interests, career plans, etc. of a student along with course details like cost, course credits, research,
;etc. Using this information and a set of rules, our expert system suggests the student with the course title.
 
;How to run the program:
; In clips: Use the following commands
;(clear)
;Load the file from (file - load)
;(reset)
;(run)
; Expert system asks some questions related to the student and his interests.
; please refer to the knowledge base below to know what to answer for a particular course as a recommendation.
; Because some combinations do not give any answer as there are no rules for that.
; It is case sensitive. Please use values given in braces for questions.


; Define classes for Student, Course and Major

(defclass STUDENT (is-a USER) (role concrete) (slot firstname) (slot research_int) (slot prereq_atd) (slot work_exp) (slot semes) (slot topper) (slot int_class) (slot alone_ppl))
(defclass MAJOR (is-a USER) (role concrete) (slot grade) (slot suggested_major) (slot group_act) (slot category) (slot undergrad_subject) (slot career) (slot events) (slot cost))
(defclass COURSE (is-a USER) (role concrete) (slot major) (slot prereq) (slot is_research) (slot suggested_course) (slot sem) (slot wk_exp))



; Take input of student name and if he already choose a major.

(defrule input_name (declare (salience 100)) 
?ins <- (object (is-a STUDENT)) 
=> 
(printout t "Please enter your name  ")
(send ?ins put-firstname (read)))

(defrule input_major (declare (salience 90)) 
?ins <- (object (is-a MAJOR)) 
=> 
(printout t "Did you already choose your major? If so what is it?(CS,IT,EE,Finance,HR,no)  ")
(send ?ins put-suggested_major (read)))



; if student dint choose a major, evaluate his intrests to come to an intermediate conclusion of suggesting major.

(defrule Major_no (declare (salience 80)) 
?ins <- (object (is-a MAJOR) (suggested_major no)) 
=> 
(printout t "Do you like working in groups and managing people(people) or do you prefer working individualy(alone)? ")
(send ?ins put-group_act (read)))


(defrule cost_factor (declare (salience 80)) 
?ins <- (object (is-a MAJOR) (group_act people)) 
=> 
(printout t "Will you be able to pay high cost for the courses? ")
(send ?ins put-cost (read)))




; We have 2 main categories here. One is business and other is Engineering. We limites the scope for easier understanding.
; relevant questions are asked based on the category intrested.

(defrule suggest_Business (declare (salience 80))
?ins <- (object (is-a MAJOR) (suggested_major no) (group_act people) (cost yes)) 
=> 
(send ?ins put-category Business) 
(printout t "Student might be interested in Business  " crlf)
(printout t "what is the class that you like most in undergrad?(Maths,Programming,LogicDesign)  ")
(send ?ins put-undergrad_subject (read))
(printout t "Have you organized any events/workshops? ")
(send ?ins put-events (read)))


(defrule suggest_Engineering  (declare (salience 80))
?ins <- (object (is-a MAJOR) (suggested_major no) (group_act alone) ) 
=> (send ?ins put-category Engineering) 
(printout t "Student might be interested in Engineering " crlf)
(printout t "what is the class that you like most in undergrad?(Maths,Programming,LogicDesign)  ")
(send ?ins put-undergrad_subject (read))
(printout t "Are you in the top 10 of your undergrad class?  ")
(send ?ins put-grade (read))
(printout t "What kind of career preferences do you have? (Software,Hardware) ")
(send ?ins put-career (read)))



; Intermediate conclusion of which major must the student choose is given based on above answers.

(defrule suggest_Finance (declare (salience 70)) 
?ins <- (object (is-a MAJOR) (category Business) (undergrad_subject Maths) ) 
=> (send ?ins put-suggested_major Finance) 
(printout t "Suggesting student to take major : Finance " crlf))

(defrule suggest_HR (declare (salience 70)) 
?ins <- (object (is-a MAJOR) (category Business) (events yes) ) 
=> (send ?ins put-suggested_major HR)
(printout t "Suggesting student to take major : HR " crlf))

(defrule suggest_CS (declare (salience 70))
?ins <- (object (is-a MAJOR) (category Engineering) (undergrad_subject Programming) (grade yes) ) 
=> (send ?ins put-suggested_major CS)
(printout t "Suggesting student to take major : Computer Science " crlf))

(defrule suggest_IT (declare (salience 70))
?ins <- (object (is-a MAJOR) (category Engineering) (career Software) (grade no) ) 
=> (send ?ins put-suggested_major IT)
(printout t "Suggesting student to take major : Information Technology " crlf))

(defrule suggest_EE (declare (salience 70))
?ins <- (object (is-a MAJOR) (category Engineering) (career Hardware) (undergrad_subject LogicDesign) ) 
=> (send ?ins put-suggested_major EE)
(printout t "Suggesting student to take major : Electrical Engineering " crlf))




; Questions are asked particular to course attributes.

(defrule input_subject (declare (salience 50))
?ins <- (object (is-a COURSE) (major CS))
=> 
(printout t "Is it your first semester? (1 /nil) ")
(send ?ins put-sem (read))
(printout t "Are you intrested in research subjects? (yes/no)  ")
(send ?ins put-is_research (read))
(printout t "Have you attended any pre-requisite courses? (AssemblyLanguage,DataAnalytics, Algorithms, MicroElectronics)  ")
(send ?ins put-prereq (read))
(printout t "Do you have previous work experience? (yes/no)  ")
(send ?ins put-wk_exp (read)))


;Course is suggested depending on the values of above attributes for course.

(defrule is_CA 
?ins <- (object (is-a COURSE) (major CS) (is_research no) (prereq AssemblyLanguage)) 
=> (send ?ins put-suggested_course ComputerArchitecture) )

(defrule is_AI 
?ins <- (object (is-a COURSE) (major CS) (is_research yes) (prereq Algorithms)) 
=> (send ?ins put-suggested_course ArtificialIntelligence) )


(defrule is_Hadoop 
?ins <- (object (is-a COURSE) (major IT) (is_research no) (sem 1)) 
=> (send ?ins put-suggested_course Hadoop) )

(defrule is_BigData
?ins <- (object (is-a COURSE) (major IT) (is_research no) (prereq DataAnalytics)) 
=> (send ?ins put-suggested_course BigData) )

(defrule is_Fincl_plan 
?ins <- (object (is-a COURSE) (major Finance) (is_research no) (sem 1)) 
=> (send ?ins put-suggested_course FinancialPlanning) )

(defrule is_Buss_eco 
?ins <- (object (is-a COURSE) (major Finance) (is_research yes) (wk_exp yes)) 
=> (send ?ins put-suggested_course BusinessEconomics) )

(defrule is_Leadership 
?ins <- (object (is-a COURSE) (major HR) (is_research no) (wk_exp yes)) 
=> (send ?ins put-suggested_course LeadershipEntrepreneurship) )

(defrule is_HRM 
?ins <- (object (is-a COURSE) (major HR) (is_research yes) (sem 1)) 
=> (send ?ins put-suggested_course HumanResourceManagement) )

(defrule is_VLSI 
?ins <- (object (is-a COURSE) (major EE) (is_research no) (sem 1) ) 
=> (send ?ins put-suggested_course VLSI) )

(defrule is_PE 
?ins <- (object (is-a COURSE) (major EE) (is_research yes) (prereq MicroElectronics)) 
=> (send ?ins put-suggested_course PowerElectronics) )


; objects are instantiated

(definstances COURSE-INSTANCES 
(subject1 of COURSE (major CS) (is_research no) (prereq AssemblyLanguage))
(subject2 of COURSE (major CS) (is_research yes) (prereq Algorithms))
(subject3 of COURSE (major IT) (is_research no) (sem 1))
(subject4 of COURSE (major IT) (is_research no) (prereq DataAnalytics))
(subject5 of COURSE (major Finance) (is_research no) (sem 1))
(subject6 of COURSE (major Finance) (is_research yes) (wk_exp yes))
(subject7 of COURSE (major HR) (is_research no) (wk_exp yes))
(subject8 of COURSE (major HR) (is_research yes) (sem 1))
(subject9 of COURSE (major EE) (is_research no) (sem 1))
(subject10 of COURSE (major EE) (is_research yes) (prereq MicroElectronics)))
(definstances MAJOR-INSTANCES 
(major1 of MAJOR))
(definstances STUDENT-INSTANCES 
(student1 of STUDENT))


; Final statement for course suggestion
(defrule choose_course (declare (salience -10)) 
(object (is-a STUDENT) (firstname ?nm)) 
(object (is-a MAJOR)   (suggested_major ?mj)) 
(object (is-a COURSE) (major ?mj) (suggested_course ?c)) => (printout t " If " ?nm " chooses " ?mj " then he/she should take " ?c " for his/her course selection." crlf))