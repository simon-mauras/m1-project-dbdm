INSERT INTO works(Person,Project)
SELECT T0.Person, 'Skolem[m0,p](' || T0.Field || ',' || T0.Person || ')'
FROM hasjob T0;
INSERT INTO area(Project,Field)
SELECT 'Skolem[m0,p](' || T0.Field || ',' || T0.Person || ')', T0.Field
FROM hasjob T0;
INSERT INTO works(Person,Project)
SELECT T0.Professor, 'Skolem[m1,p](' || T1.Field || ',' || T0.Professor || ')'
FROM teaches T0 INNER JOIN inField T1
ON T1.Course = T0.Course;
INSERT INTO area(Project,Field)
SELECT 'Skolem[m1,p](' || T1.Field || ',' || T0.Professor || ')', T1.Field
FROM teaches T0 INNER JOIN inField T1
ON T1.Course = T0.Course;
INSERT INTO works(Person,Project)
SELECT T0.Researcher, T1.Project
FROM get T0 INNER JOIN forGrant T1
ON T1.Grant = T0.Grant;
