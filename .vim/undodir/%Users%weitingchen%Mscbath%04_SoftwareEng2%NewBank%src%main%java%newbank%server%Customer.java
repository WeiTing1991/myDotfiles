Vim�UnDo� ��l���{B�����=4�ߊ�t�/��+8                                      e���    _�                              ����                                                                                                                                                                                                                                                                                                                                                             e���    �         ,      	   %	private ArrayList<Account> accounts;   	   	public Customer() {   		accounts = new ArrayList<>();   	}   	   #	public String accountsToString() {   		String s = "";   		for(Account a : accounts) {   			s += a.toString();   		}   		return s;   	}    �         .      *	public void addAccount(Account account) {   		accounts.add(account);		   	}�      .         /	// method to return accounts of customer   -AG   )	public ArrayList<Account> getAccounts(){   		return accounts;   	}5��                       �      n       �      �                     &   \      H       &       �                          {       �               5��