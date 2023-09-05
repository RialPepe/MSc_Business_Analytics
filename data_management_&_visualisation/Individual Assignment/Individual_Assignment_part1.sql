CREATE DATABASE Individual_assignment;


use individual_assignment;


CREATE TABLE registration_office (
student_id int NOT NULL,
first_name varchar(255),
last_name varchar(255),
addres varchar(255),
email varchar(255),
phone_number int,
program_name varchar(255),
PRIMARY KEY(student_id)); 

INSERT INTO registration_office (student_id, first_name, last_name, addres, email, phone_number, program_name) 
	VALUES (001, "José Ramón", "Cobos", "Avenida de las águilas", "jrc@tcd.ie", 666666666, "Business Analytics");
    
INSERT INTO registration_office (student_id, first_name, last_name, addres, email, phone_number, program_name) 
	VALUES (002, "Álvaro", "Tapia", "Avenida de los mamuts", "at@tcd.ie", 555555555, "Business Analytics");

INSERT INTO registration_office (student_id, first_name, last_name, addres, email, phone_number, program_name) 
	VALUES (003, "Niccolo", "nolosé", "Avenida de los espaguetis", "nn@tcd.ie", 444444444, "Business Analytics");

INSERT INTO registration_office (student_id, first_name, last_name, addres, email, phone_number, program_name) 
	VALUES (004, "Ivan", "nolosé2", "Avenida de los rusos", "ivn@tcd.ie", 333333333, "Business Analytics");

INSERT INTO registration_office (student_id, first_name, last_name, addres, email, phone_number, program_name) 
	VALUES (005, "Sandra", "Calero", "Avenida de los españoles", "sc@tcd.ie", 222222222, "Marketing digital");
    
    

CREATE TABLE hr_office (
staff_id int,
position varchar(255),
salary int,
staff_name varchar(255),
PRIMARY KEY(staff_id));

INSERT INTO hr_office (staff_id, position, salary, staff_name) 
	VALUES (001, "professor", 20000, "Nicholas");

INSERT INTO hr_office (staff_id, position, salary, staff_name) 
	VALUES (002, "professor", 20000, "Venu");

INSERT INTO hr_office (staff_id, position, salary, staff_name) 
	VALUES (003, "professor", 20000, "Ashish");
    
 INSERT INTO hr_office (staff_id, position, salary, staff_name) 
	VALUES (004, "secretary", 20000, "María");   

INSERT INTO hr_office (staff_id, position, salary, staff_name) 
	VALUES (005, "secretary", 20000, "Mario");


CREATE TABLE course_detail (
course_name varchar(255),
staff_id int,
max_student int,
primary key (course_name),
foreign key (staff_id) references hr_office (staff_id));

INSERT INTO course_detail (course_name, staff_id, max_student) 
	VALUES ("business forecasting", 001, 20);

INSERT INTO course_detail (course_name, staff_id, max_student) 
	VALUES ("business data mining", 001, 20);

INSERT INTO course_detail (course_name, staff_id, max_student) 
	VALUES ("strategy", 001, 20);

INSERT INTO course_detail (course_name, staff_id, max_student) 
	VALUES ("ethics", 002, 20);
    
INSERT INTO course_detail (course_name, staff_id, max_student) 
	VALUES ("foundations", 003, 20);


CREATE TABLE complaint_detail (
student_id int,
complaint_id int,
staff_id int,
facility_name varchar(255),
course_name varchar(255),
resolution varchar(255),
PRIMARY KEY(complaint_id),
foreign key (student_id) references registration_office (student_id),
foreign key (staff_id) references hr_office (staff_id),
foreign key (course_name) references course_detail (course_name));

INSERT INTO complaint_detail (student_id, complaint_id, staff_id, facility_name, course_name, resolution) 
	VALUES (001, 001, 001, "las clases son pequeñas", "business forecasting", "OK, we are going to work on it");
    
INSERT INTO complaint_detail (student_id, complaint_id, staff_id, facility_name, course_name, resolution) 
	VALUES (003, 002, 001, "las sillas son pequeñas", "business forecasting", "OK, we are going to work on it");
    
INSERT INTO complaint_detail (student_id, complaint_id, staff_id, facility_name, course_name, resolution) 
	VALUES (002, 003, 001, "el micro no funcionab bien", "business forecasting", "OK, i will buy a new one");
    
INSERT INTO complaint_detail (student_id, complaint_id, staff_id, facility_name, course_name, resolution) 
	VALUES (004, 005, 003, "hay poca luz en las clases", "strategy", "OK, will buy more lights");
    
INSERT INTO complaint_detail (student_id, complaint_id, staff_id, facility_name, course_name, resolution) 
	VALUES (005, 004, 003, "hay una ventana rota", "strategy", "OK, we will fix it");
    
    
CREATE TABLE course_registry (
student_id int,
course_name varchar(255),
foreign key (student_id) references registration_office (student_id),
foreign key (course_name) references course_detail (course_name));

INSERT INTO course_registry (student_id, course_name) 
	VALUES (001, "foundations");
    
INSERT INTO course_registry (student_id, course_name) 
	VALUES (001, "strategy");

INSERT INTO course_registry (student_id, course_name) 
	VALUES (001, "ethics");

INSERT INTO course_registry (student_id, course_name) 
	VALUES (004, "ethics");
    
INSERT INTO course_registry (student_id, course_name) 
	VALUES (005, "strategy");