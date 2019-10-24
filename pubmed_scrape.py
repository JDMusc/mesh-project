import pandas as pd
import xml.etree.ElementTree as ET 

def loadXml(f_name):
    tree = ET.parse(f_name)
    return (tree, tree.getroot())


def getRemoveTypes():
    tys = ['Annotation', 'EntryCombinationList',
            'CASN1name', 'ConceptList',
            'DateCreated', 'DateRevised', 'DateEstablished',
            'AllowableQualifiersList', 'HistoryNote', 'OnlineNote',
            'PublicMeSHNote', 'PreviousIndexingList',
            'PharmacologicalActionList', 'SeeRelatedList',
            'relatedregistrynumberlist', 'registrynumber',
            'scopenote', 'thesaurusidlist',
            'NLMClassificationNumber'] 
    return tys


def removeChildren(root, types = getRemoveTypes()):
    for dr in root.findall('DescriptorRecord'):
        for ty in types:
            ty_els = dr.findall(ty)
            if(len(ty_els) == 0):
                continue;

            for ty_el in ty_els:
                dr.remove(ty_el)
    
    return root


def compressPubmedXml(src_f, dest_f):
    (tree, root) = loadXml(src_f)
    root = removeChildren(root)
    tree.write(dest_f)


def descriptorsAsDataFrame(root_xml, dest_f = None):
    if type(root_xml) is str:
        (_, root_xml) = loadXml(root_xml)

    df = pd.DataFrame(flattenRecords([r for r in root_xml]), 
            columns = ["UI", "Descriptor", "TreeNumber"])

    if dest_f is not None:
        df.to_csv(dest_f, index = None)

    return df


def flattenRecords(records):
    return [(ui, name, tn) for rec in records for (ui, name, tn) in flattenRecord(rec)]


def flattenRecord(record):
    def childOrBlank(path):
        child = record.find(path)
        child = child.text if child is not None else ""
        return child

    ui = childOrBlank("DescriptorUI")
    name = childOrBlank("DescriptorName/String")

    tns = record.findall("TreeNumberList/TreeNumber")
    if len(tns) == 0:
        return [(ui, name, "")]

    return [(ui, name, tn.text) for tn in tns]

